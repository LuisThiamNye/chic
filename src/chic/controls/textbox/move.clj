(ns chic.controls.textbox.move
  (:require
   [proteus :refer [let-mutable]]
   [chic.controls.textbox.helper :as hpr]
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
   [clj-commons.primitive-math :as prim]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser]
   [chic.bifurcan :as b])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)
   (com.ibm.icu.text BreakIterator)))

(defn ^BreakIterator make-word-iter [rope]
  (doto (BreakIterator/getWordInstance)
    (.setText (b/rope-character-iterator rope))))

;; this implementation gives same word-movement results as macOS TextEdit

(defn word-before [^BreakIterator iter idx]
  (loop [idx' (.following iter (dec idx))
         found-word? false]
    (when (prim/<= 0 idx')
      (if (or (prim/zero? idx') found-word?)
        idx'
        (let [rs (.getRuleStatus iter)]
          (recur (.previous iter)
                 (or (prim/< rs BreakIterator/WORD_NONE)
                     (prim/<= BreakIterator/WORD_NONE_LIMIT rs))))))))

(defn word-after [^BreakIterator iter idx]
  (let [n (.last iter)]
    (loop [idx' (.following iter idx)]
      (when (prim/<= 0 idx')
        (if (or (prim/== n idx')
                (let [rs (.getRuleStatus iter)]
                 ;; if not punctuation/space
                  (or (prim/< rs BreakIterator/WORD_NONE)
                      (prim/<= BreakIterator/WORD_NONE_LIMIT rs))))
          idx'
          (recur (.next iter)))))))

(comment
  (.following (make-word-iter (Rope/from "012 45 78")) 9) ;; -1

  ;; getRuleStatus is 0 at WORD_NONE boundaries
  ;; type of boundary defined by the index before
  ;; eg idx=3 "012| 45" gives number rule status (100)
  ;; eg idx=0 or idx=4 "012 |45" gives WORD_NONE rule status (100)
  ;; WORD_NONE includes space & punctuation and does not distinguish between them
  (.getRuleStatus
   (doto (make-word-iter (Rope/from "012 45 78"))
     (.following 6)))
  (.getRuleStatus
   (doto (make-word-iter (Rope/from "012  5 78"))
     (.preceding 2)))

  ;; there is a boundary between each instance of:
  ;;    newline = , .
  ;; & other punctuation, but spaces are contiguous
  (.following (make-word-iter (Rope/from "...")) 0)

  ;; It may be feasible to create an interator with a set of
  ;; custom compiled rules
  (type (BreakIterator/getWordInstance)) ;; RuleBasedBreakIterator
  (str (BreakIterator/getWordInstance)) ;; prints the rule source

  #!
  )

(defn move-cursor-to-coord* [state {:keys [x y]}]
  (let [{:keys [line-start-idxs text-lines]
         {:keys [line-height first-line-origin]} :layout} state
        dy (- y (:y first-line-origin))
        lidx (long (Math/floor (/ dy line-height)))
        nlines (count line-start-idxs)
        lidx (max 0 (min (dec nlines) lidx))
        text-line ^TextLine (nth text-lines lidx)
        dx (- x (:x first-line-origin))
        cidx (.getOffsetAtCoord text-line dx)]
    (-> state
        (assoc :cursor-dx (.getCoordAtOffset text-line cidx))
        (assoc :cursor-idx (+ (nth line-start-idxs lidx) cidx))
        (assoc :cursor-line-idx lidx))))

(defn cursor-move-right* [{:keys [cursor-idx ^Rope rope line-start-idxs
                                  cursor-line-idx] :as state}]
  (let [next-line-idx (inc cursor-line-idx)]
    (-> state
        (assoc :cursor-idx (min (.size rope) (inc cursor-idx)))
        (cond-> (<= (nth line-start-idxs next-line-idx Long/MAX_VALUE) (inc cursor-idx))
          (assoc :cursor-line-idx next-line-idx))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-left* [{:keys [cursor-idx line-start-idxs
                                 cursor-line-idx] :as state}]
  (let [cursor-idx2 (max 0 (dec cursor-idx))]
    (-> state
        (assoc :cursor-idx cursor-idx2)
        (cond-> (< cursor-idx2 (nth line-start-idxs cursor-line-idx))
          (assoc :cursor-line-idx (dec cursor-line-idx)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-to-idx* [{:keys [cursor-idx ^Rope rope line-start-idxs
                                   cursor-line-idx] :as state} cursor-idx2]
  (let [line-idx2 (hpr/find-line-idx line-start-idxs cursor-idx2)]
    (-> state
        (assoc :cursor-idx cursor-idx2)
        (cond-> (not (== cursor-line-idx line-idx2))
          (assoc :cursor-line-idx line-idx2))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-right-word* [{:keys [cursor-idx ^Rope rope line-start-idxs
                                       cursor-line-idx] :as state}]
  (let [word-iter (make-word-iter rope)
        cursor-idx2 (word-after word-iter cursor-idx)]
    (if cursor-idx2
      (cursor-move-to-idx* state cursor-idx2)
      state)))

(defn cursor-move-left-word* [{:keys [cursor-idx ^Rope rope line-start-idxs
                                      cursor-line-idx] :as state}]
  (let [word-iter (make-word-iter rope)
        cursor-idx2 (word-before word-iter cursor-idx)]
    (if cursor-idx2
      (cursor-move-to-idx* state cursor-idx2)
      state)))

(defn cursor-move-down* [{:keys [cursor-target-dx ^Rope rope line-start-idxs
                                 cursor-line-idx] :as state}]
  (let [next-line-idx (inc cursor-line-idx)
        has-next? (< next-line-idx (count line-start-idxs))]
    (-> state
        (cond-> has-next?
          (-> (assoc :cursor-line-idx next-line-idx)
              (as-> state (assoc state :cursor-idx (hpr/dx->cursor-idx state cursor-target-dx)))))
        (cond-> (not has-next?)
          (assoc :cursor-idx (.size rope)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-up* [{:keys [cursor-target-dx ^Rope rope line-start-idxs
                               cursor-line-idx] :as state}]
  (let [next-line-idx (dec cursor-line-idx)
        has-prev? (<= 0 next-line-idx)]
    (-> state
        (cond-> has-prev?
          (-> (assoc :cursor-line-idx next-line-idx)
              (as-> state (assoc state :cursor-idx (hpr/dx->cursor-idx state cursor-target-dx)))))
        (cond-> (not has-prev?)
          (assoc :cursor-idx 0))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-start* [{:keys [line-start-idxs
                                  cursor-line-idx] :as state}]
  (-> state
      (assoc :cursor-idx (nth line-start-idxs cursor-line-idx))
      (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))

(defn cursor-move-end* [{:keys [^Rope rope line-start-idxs
                                cursor-line-idx] :as state}]
  (-> state
      (assoc :cursor-idx (dec (nth line-start-idxs (inc cursor-line-idx)
                                   (inc (.size rope)))))
      (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))

(defn cursor-move-start-up* [{:keys [line-start-idxs cursor-idx
                                     cursor-line-idx] :as state}]
  (if (== cursor-idx (nth line-start-idxs cursor-line-idx))
    (cursor-move-up* state)
    (cursor-move-start* state)))

(defn cursor-move-end-down* [{:keys [^Rope rope line-start-idxs
                                     cursor-line-idx cursor-idx] :as state}]
  (if (== cursor-idx (dec (nth line-start-idxs (inc cursor-line-idx)
                               (inc (.size rope)))))
    (-> state cursor-move-down* cursor-move-end*)
    (cursor-move-end* state)))

(defn cursor-move-doc-start* [{:keys [] :as state}]
  (-> state
      (assoc :cursor-idx 0)
      (assoc :cursor-line-idx 0)
      (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))

(defn cursor-move-doc-end* [{:keys [^Rope rope line-start-idxs] :as state}]
  (-> state
      (assoc :cursor-idx (.size rope))
      (assoc :cursor-line-idx (dec (count line-start-idxs)))
      (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))

(defn handle-move-intent [*state intent]
  (case intent
    :move-right
    (vswap! *state cursor-move-right*)
    :move-left
    (vswap! *state cursor-move-left*)
    :move-right-word
    (vswap! *state cursor-move-right-word*)
    :move-left-word
    (vswap! *state cursor-move-left-word*)
    :move-down
    (vswap! *state cursor-move-down*)
    :move-up
    (vswap! *state cursor-move-up*)
    :move-start
    (vswap! *state cursor-move-start*)
    :move-end
    (vswap! *state cursor-move-end*)
    :move-start-up
    (vswap! *state cursor-move-start-up*)
    :move-end-down
    (vswap! *state cursor-move-end-down*)
    :move-doc-start
    (vswap! *state cursor-move-doc-start*)
    :move-doc-end
    (vswap! *state cursor-move-doc-end*)
    nil))
