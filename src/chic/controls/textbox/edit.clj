(ns chic.controls.textbox.edit
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
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser]
   [chic.controls.textbox.move :as move])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)))

(defn insert-text* [{:keys [cursor-line-idx line-start-idxs cursor-idx] :as state} ^String text]
  (let [rope (.insert ^Rope (:rope state)
                      (int (:cursor-idx state))
                      text)
        text-lines (hpr/rope->textlines state rope)
        nchars (count text)
        cursor-idx2 (+ cursor-idx nchars)
        lf-idxs (loop [idx (.indexOf text 10)
                       idxs []]
                  (if (<= 0 idx)
                    (recur (.indexOf text 10 (unchecked-inc-int idx))
                           (conj idxs idx))
                    idxs))
        nextralines (count lf-idxs)]
    (-> state
        (assoc :cursor-idx cursor-idx2)
        (cond-> (< 0 nextralines)
          (assoc :cursor-line-idx (+ nextralines cursor-line-idx)))
        (assoc :line-start-idxs
               (-> (subvec line-start-idxs 0 (inc cursor-line-idx))
                   (into (map #(+ % cursor-idx 1)) lf-idxs)
                   (into (map #(+ % nchars)
                              (subvec line-start-idxs (inc cursor-line-idx))))))
        (assoc :rope rope)
        (assoc :text-lines text-lines)
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn delete-left* [{:keys [cursor-idx ^Rope rope line-start-idxs
                            cursor-line-idx] :as state}]
  (if (== 0 cursor-idx)
    state
    (let [cursor-idx2 (dec cursor-idx)
          rope2 (.remove rope cursor-idx2 cursor-idx)
          moved-line? (< cursor-idx2 (nth line-start-idxs cursor-line-idx))]
      (-> state
          (assoc :cursor-idx cursor-idx2)
          (assoc :rope rope2)
          (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
          (cond-> moved-line?
            (-> (assoc :cursor-line-idx (dec cursor-line-idx))
                (assoc :line-start-idxs (into (subvec line-start-idxs 0 cursor-line-idx)
                                              (map dec)
                                              (subvec line-start-idxs (inc cursor-line-idx))))))
          (cond-> (not moved-line?)
            (assoc :line-start-idxs (into (subvec line-start-idxs 0 (inc cursor-line-idx))
                                          (map dec)
                                          (subvec line-start-idxs (inc cursor-line-idx)))))
          (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))

(defn delete-right* [{:keys [cursor-idx ^Rope rope line-start-idxs
                             cursor-line-idx] :as state}]
  (if (== cursor-idx (.size rope))
    state
    (let [delete-idx (inc cursor-idx)
          rope2 (.remove rope cursor-idx delete-idx)
          joined-line? (<= (nth line-start-idxs (inc cursor-line-idx) Long/MAX_VALUE) delete-idx)]
      (-> state
          (assoc :rope rope2)
          (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
          (cond-> joined-line?
            (assoc :line-start-idxs (into (subvec line-start-idxs 0 (inc cursor-line-idx))
                                          (map dec)
                                          (subvec line-start-idxs (inc (inc cursor-line-idx))))))
          (cond-> (not joined-line?)
            (assoc :line-start-idxs (into (subvec line-start-idxs 0 (inc cursor-line-idx))
                                          (map dec)
                                          (subvec line-start-idxs (inc cursor-line-idx)))))))))

(defn delete-between* [{:keys [^Rope rope line-start-idxs] :as state} idx1 idx2]
  (let [rope2 (.remove rope idx1 idx2)
        nchars (- idx2 idx1)
        upper-line-idx (hpr/find-line-idx line-start-idxs idx1)
        lower-line-idx (hpr/find-line-idx line-start-idxs idx2)]
    (-> state
        (assoc :rope rope2)
        (assoc :line-start-idxs (into (subvec line-start-idxs 0 (inc upper-line-idx))
                                      (map #(- % nchars))
                                      (subvec line-start-idxs (inc lower-line-idx))))
        (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn delete-start* [{:keys [cursor-idx ^Rope rope line-start-idxs
                             cursor-line-idx] :as state}]
  (let [line-start-idx (nth line-start-idxs cursor-line-idx)]
    (if (== cursor-idx line-start-idx)
      (delete-left* state)
      (-> state
          (assoc :cursor-idx line-start-idx)
          (delete-between* line-start-idx cursor-idx)))))

(defn delete-end* [{:keys [cursor-idx ^Rope rope line-start-idxs
                           cursor-line-idx] :as state}]
  (let [line-end-idx (dec (nth line-start-idxs (inc cursor-line-idx)
                               (inc (.size rope))))]
    (if (== cursor-idx line-end-idx)
      (delete-right* state)
      (-> state (delete-between* cursor-idx line-end-idx)))))

(defn enter-newline* [{:keys [^long cursor-idx ^Rope rope line-start-idxs
                              cursor-line-idx] :as state}]
  (let [cursor2 (inc cursor-idx)
        line-idx2 (inc cursor-line-idx)
        rope2 (.insert rope cursor-idx "\n")]
    (-> state
        (assoc :rope rope2)
        (assoc :cursor-line-idx line-idx2)
        (assoc :cursor-idx cursor2)
        (assoc :line-start-idxs (into (conj (subvec line-start-idxs 0 line-idx2)
                                            cursor2)
                                      (map inc)
                                      (subvec line-start-idxs line-idx2)))
        (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn handle-edit-intent [*state intent]
  (case intent
    :delete-left
    (vswap! *state delete-left*)
    :delete-right
    (vswap! *state delete-right*)
    :delete-right-word
    (vswap! *state (fn [{:keys [cursor-idx] :as state}]
                     (let [idx2 (move/word-after (move/make-word-iter (:rope state))
                                                 cursor-idx)]
                       (-> state
                           (delete-between* cursor-idx idx2)
                           (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))
    :delete-left-word
    (vswap! *state (fn [{:keys [cursor-idx] :as state}]
                     (let [idx2 (move/word-before (move/make-word-iter (:rope state))
                                                  cursor-idx)]
                       (-> state
                           (assoc :cursor-idx idx2)
                           (assoc :cursor-line-idx (hpr/find-line-idx (:line-start-idxs state) idx2))
                           (delete-between* idx2 cursor-idx)
                           (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))
    :delete-start
    (vswap! *state delete-start*)
    (:kill :delete-end)
    (vswap! *state delete-end*)
    :enter
    (vswap! *state enter-newline*)
    (:paste :yank)
    (when-some [text (clipboard/get :text:plain)]
      (vswap! *state insert-text* text))
    :copy
    (let [{:keys [cursor-line-idx line-start-idxs ^Rope rope]} @*state]
      (clipboard/set-text (str (.slice rope (nth line-start-idxs cursor-line-idx)
                                       (dec (nth line-start-idxs (inc cursor-line-idx) (inc (.size rope)))))
                               \newline))
      true)
    :cut
    (vswap! *state
            (fn [{:keys [cursor-line-idx line-start-idxs ^Rope rope] :as state}]
              (let [line-start-idx (nth line-start-idxs cursor-line-idx)
                    line-lf-idx (dec (nth line-start-idxs (inc cursor-line-idx) (inc (.size rope))))
                    nlines (count line-start-idxs)
                    end-line? (== (dec nlines) cursor-line-idx)
                    cursor-idx2 (max 0 (cond-> line-start-idx end-line? dec))
                    rope2 (.remove rope cursor-idx2 (min (inc line-lf-idx) (.size rope)))
                    char-count (- (.size rope) (.size rope2))]
                (clipboard/set-text
                 (str (.slice rope line-start-idx line-lf-idx)
                      \newline))
                (-> state
                    (assoc :rope rope2)
                    (assoc :cursor-idx cursor-idx2)
                    (cond-> (< 1 nlines)
                      (->
                       (cond-> end-line?
                         (update :cursor-line-idx dec))
                       (assoc :line-start-idxs (if end-line?
                                                 (pop line-start-idxs)
                                                 (into (subvec line-start-idxs 0 (inc cursor-line-idx))
                                                       (map #(- % char-count))
                                                       (subvec line-start-idxs (inc (inc cursor-line-idx))))))))
                    (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
                    (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))
    :transpose
    (vswap! *state
            (fn [{:keys [cursor-idx ^Rope rope line-start-idxs
                         cursor-line-idx] :as state}]
              (let [shift (if (== (inc cursor-idx)
                                  (nth line-start-idxs (inc cursor-line-idx) (inc (.size rope))))
                            0 1)
                    idx1 (+ cursor-idx shift -2)]
                (if (< idx1 0)
                  state
                  (let [idx2 (+ cursor-idx shift -1)
                        cint1 (.nth rope idx1)
                        cint2 (.nth rope idx2)
                        rope2 (.insert (.remove rope idx1 (inc idx2))
                                       (int idx1) (str (char cint2) (char cint1)))]
                    (-> state
                        (assoc :cursor-idx (+ cursor-idx shift))
                        (assoc :rope rope2)
                        (cond-> (== 10 cint1)
                          (update :line-start-idxs update cursor-line-idx inc))
                        (cond-> (== 10 cint2)
                          (update :line-start-idxs update cursor-line-idx dec))
                        (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
                        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))))
    nil))
