(ns chic.controls.textbox.helper
  (:require
   [proteus :refer [let-mutable]]
   [chic.clipboard :as clipboard]
   [chic.debug]
   [taoensso.encore :as enc]
   [chic.controls.textbox.cursor :as cursor]
   [potemkin :refer [doit]]
   [chic.controls.textbox.keybindings :as keybindings]
   [chic.style :as style]
   [chic.ui2.event :as ievt]
   [chic.ui.font :as uifont]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)))

(defn calc-line-start-idxs-for-changed-line
  [{:keys [line-start-idxs cursor-line-idx]} nadded]
  (into (subvec line-start-idxs 0 (inc cursor-line-idx))
        (map #(+ % nadded)
             (subvec line-start-idxs (inc cursor-line-idx)))))

(defn dx->cursor-idx [{:keys [line-start-idxs text-lines cursor-line-idx]} dx]
  (let [text-line ^TextLine (nth text-lines cursor-line-idx)]
    (+ (nth line-start-idxs cursor-line-idx)
       (.getOffsetAtCoord text-line dx))))

(defn calc-cursor-dx [{:keys [cursor-line-idx text-lines line-start-idxs cursor-idx]}]
  (.getCoordAtOffset
   ^TextLine (nth text-lines cursor-line-idx)
   (- cursor-idx (nth line-start-idxs cursor-line-idx))))

(defn rope-visual-lines [^Rope rope]
  (reify
    clojure.lang.Counted
    (count [this] (.reduce this (fn [n _] (inc n)) 0))
    clojure.lang.IReduceInit
    (reduce [_ rf init]
      (let [iter (.codePoints rope)
            sb (StringBuilder.)]
        (if-not (.hasNext iter)
          (rf init "")
          (loop [acc init
                 cint (.nextInt iter)
                 ntrailinglfs 0]
            (if (== 10 cint)
              (let [ret (rf acc (.toString sb))]
                (.setLength sb 0)
                (if (reduced? ret)
                  @ret
                  (if (.hasNext iter)
                    (recur ret (.nextInt iter) (unchecked-inc ntrailinglfs))
                    (rf ret ""))))
              (do
                (.appendCodePoint sb cint)
                (if (.hasNext iter)
                  (recur acc (.nextInt iter) 0)
                  (rf acc (.toString sb)))))))))))
(comment
  (= [""] (vec (rope-visual-lines (Rope/from ""))))
  (= ["ab"] (vec (rope-visual-lines (Rope/from "ab"))))
  (= ["a" "b"] (vec (rope-visual-lines (Rope/from "a\nb"))))
  (= ["a" "" "b"] (vec (rope-visual-lines (Rope/from "a\n\nb"))))
  (= ["a" "" "" ""] (vec (rope-visual-lines (Rope/from "a\n\n\n"))))
  (= ["" "a" "" ""] (vec (rope-visual-lines (Rope/from "\na\n\n")))))

(defn calc-line-start-idxs [^Rope rope]
  (let [producer
        (reify
          clojure.lang.IReduceInit
          (reduce [_ rf init]
            (let [iter (.codePoints rope)
                  ret (rf init 0)]
              (cond
                (reduced? ret) @ret
                (not (.hasNext iter)) ret
                :else
                (loop [acc ret
                       idx 0]
                  (if (.hasNext iter)
                    (let [idx' (unchecked-inc idx)
                          cint (.nextInt iter)]
                      (if (== 10 cint)
                        (let [ret (rf acc idx')]
                          (if (reduced? ret)
                            @ret
                            (recur ret idx')))
                        (recur acc idx')))
                    acc))))))]
    (vec producer)))

(comment
  (= [0] (calc-line-start-idxs (Rope/from "abc")))
  (= [0] (calc-line-start-idxs (Rope/from "")))
  (= [0 1] (calc-line-start-idxs (Rope/from "\n")))
  (= [0 2 5 6 9] (calc-line-start-idxs (Rope/from "0\n23\n\n67\n9")))
  #!
  )

(defn rope->textlines [{:keys [font]} ^Rope rope]
  (mapv #(uifont/shape-line-default font %) (rope-visual-lines rope)))

(defn find-line-idx [line-start-idxs cidx]
  (let [pred #(<= % cidx)
        phi-1 (/ (- (Math/sqrt 5) 1) 2)]
   (loop [min-idx -1
          rang (count line-start-idxs)]
     (if (== 0 rang)
       min-idx
       (let [idx (+ min-idx (int (Math/ceil (* phi-1 rang))))
             item (nth line-start-idxs idx)]
         (if (pred item)
           (recur idx (- (+ rang min-idx) idx))
           (recur min-idx (- idx min-idx 1))))))))

(defn recalculate-derived* [{:keys [cursor-idx rope] :as state}]
  (-> state
      (assoc :line-start-idxs (calc-line-start-idxs rope))
      (as-> state (assoc state :cursor-line-idx
                         (find-line-idx (:line-start-idxs state) cursor-idx)))
      (as-> state (assoc state :text-lines (rope->textlines state rope)))
      (as-> state (let [dx (calc-cursor-dx state)]
                    (-> state
                        (assoc :cursor-dx dx)
                        (assoc :cursor-target-dx dx))))))

(defn -dbg-check-state [{:keys [cursor-idx ^Rope rope line-start-idxs
                                cursor-line-idx text-lines] :as state}]
  (let [nlines (count (rope-visual-lines rope))
        errors (into []
                     (remove peek)
                     [[1 (<= cursor-idx (.size rope))]
                      [2 (<= 0 cursor-idx)]
                      [3 (== nlines (count line-start-idxs))]
                      [4 (< cursor-line-idx nlines)]
                      [5 (== nlines (count text-lines))]
                      ["line-start-idxs points to chars after lfs"
                       (every? (fn [idx]
                                 (or (== 0 idx)
                                     (== 10 (.nth rope (dec idx)))))
                               line-start-idxs)]
                      [7 (== 0 (nth line-start-idxs 0))]])]
    (when (pos? (count errors))
      (chic.debug/println-main errors "\nnlines" nlines)
      (chic.debug/println-main (pr-str state)))))
