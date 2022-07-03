(ns chic.controls.textbox.select
  (:require
   [proteus :refer [let-mutable]]
   [chic.controls.textbox.helper :as hpr]
   [chic.controls.textbox.move :as move]
   [chic.controls.textbox.edit :as edit]
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

(defn select-move-generic [{:keys [cursor-idx select-idx] :as state} move-cursor]
  (let [state2 (move-cursor state)]
    (cond
      (nil? select-idx)
      (assoc state2 :select-idx cursor-idx)
      (== select-idx (:cursor-idx state2))
      (assoc state2 :select-idx nil)
      :else
      state2)))

(defn handle-select-intent [*state intent]
  (case intent
    :select-right
    (vswap! *state select-move-generic move/cursor-move-right*)
    :select-left
    (vswap! *state select-move-generic move/cursor-move-left*)
    :select-down
    (vswap! *state select-move-generic move/cursor-move-down*)
    :select-up
    (vswap! *state select-move-generic move/cursor-move-up*)
    :select-start
    (vswap! *state select-move-generic move/cursor-move-start*)
    :select-end
    (vswap! *state select-move-generic move/cursor-move-end*)
    :select-doc-start
    (vswap! *state select-move-generic move/cursor-move-doc-start*)
    :select-doc-end
    (vswap! *state select-move-generic move/cursor-move-doc-end*)
    :select-all
    (vswap! *state
            (fn [{:keys [^Rope rope line-start-idxs]:as state}]
              (cond-> state
                (< 0 (.size rope))
                (-> (assoc :cursor-idx (.size rope))
                    (assoc :select-idx 0)
                    (assoc :cursor-line-idx (dec (count line-start-idxs)))
                    (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state)))))))
    nil))

(defn move-to-sel-limit [state right?]
  (let [idx2 ((if right? max min) (:cursor-idx state) (:select-idx state))]
    (-> state
        (assoc :select-idx nil)
        (assoc :cursor-idx idx2)
        (assoc :cursor-line-idx (hpr/find-line-idx (:line-start-idxs state) idx2))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn handle-deselecting-move-intent [*state intent]
  (let [simple-move (fn [state move-cursor]
                      (-> state
                          (assoc :select-idx nil)
                          move-cursor))]
    (case intent
     :move-right
     (vswap! *state move-to-sel-limit true)
     :move-left
     (vswap! *state move-to-sel-limit false)
     :move-down
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-sel-limit true)
                          move/cursor-move-down*)))
     :move-up
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-sel-limit false)
                          move/cursor-move-up*)))
     :move-start
     (vswap! *state simple-move move/cursor-move-start*)
     :move-end
     (vswap! *state simple-move move/cursor-move-end*)
     :move-doc-start
     (vswap! *state simple-move move/cursor-move-doc-start*)
     :move-doc-end
     (vswap! *state simple-move move/cursor-move-doc-end*)
     nil)))

(defn delete-sel-contents* [{:keys [cursor-idx ^Rope rope line-start-idxs
                                    cursor-line-idx select-idx] :as state}]
  (let [idxmin (min cursor-idx select-idx)
        idxmax (max cursor-idx select-idx)
        rope2 (.remove rope idxmin idxmax)
        nchars (- idxmax idxmin)
        moved-line? (< idxmin (nth line-start-idxs cursor-line-idx))
        select-line-idx (hpr/find-line-idx line-start-idxs select-idx)
        upper-line-idx (if moved-line?
                         select-line-idx cursor-line-idx)
        lower-line-idx (if moved-line?
                         cursor-line-idx select-line-idx)]
    (-> state
        (assoc :select-idx nil)
        (assoc :cursor-idx idxmin)
        (assoc :rope rope2)
        (cond-> moved-line?
          (-> (assoc :cursor-line-idx upper-line-idx)))
        (assoc :line-start-idxs (into (subvec line-start-idxs 0 (inc upper-line-idx))
                                      (map #(- % nchars))
                                      (subvec line-start-idxs (inc lower-line-idx))))
        (as-> state (assoc state :text-lines (hpr/rope->textlines state rope2)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn handle-selected-edit-intent [*state intent]
  (let [copy-sel (fn [{:keys [^Rope rope cursor-idx select-idx]}]
                   (let [idxmin (min cursor-idx select-idx)
                         idxmax (max cursor-idx select-idx)]
                     (clipboard/set-text (.toString (.slice rope idxmin idxmax)))))]
    (case intent
     (:delete-left :delete-left-word :delete-right :delete-right-word
                   :kill)
     (vswap! *state delete-sel-contents*)
     :delete-start
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-sel-limit true)
                          edit/delete-start*)))
     :delete-end
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-sel-limit false)
                          edit/delete-end*)))
     :enter
     (vswap! *state #(-> % delete-sel-contents* edit/enter-newline*))
     (:paste :yank)
     (when-some [text (clipboard/get :text:plain)]
       (vswap! *state #(-> % delete-sel-contents* (edit/insert-text* text))))
     :copy
     (do (copy-sel @*state)
         true)
     :cut
     (vswap! *state #(-> % (doto copy-sel) delete-sel-contents*))
     nil)))
