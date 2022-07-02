(ns chic.controls.textbox.select
  (:require
   [proteus :refer [let-mutable]]
   [chic.controls.textbox.helper :as hpr]
   [chic.controls.textbox.move :as move]
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

(defn handle-deselecting-move-intent [*state intent]
  (let [simple-move (fn [state move-cursor]
                      (-> state
                          (assoc :select-idx nil)
                          move-cursor))
        move-to-limit
        (fn [state right?]
          (let [idx2 ((if right? max min) (:cursor-idx state) (:select-idx state))]
            (-> state
               (assoc :select-idx nil)
               (assoc :cursor-idx idx2)
               (assoc :cursor-line-idx (hpr/find-line-idx (:line-start-idxs state) idx2))
               (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))]
    (case intent
     :move-right
     (vswap! *state move-to-limit true)
     :move-left
     (vswap! *state move-to-limit false)
     :move-down
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-limit true)
                          move/cursor-move-down*)))
     :move-up
     (vswap! *state (fn [state]
                      (-> state
                          (move-to-limit false)
                          move/cursor-move-up*)))
     :move-start
     (vswap! *state simple-move move/cursor-move-start*)
     :move-end
     (vswap! *state simple-move move/cursor-move-end*)
     :move-doc-start
     (vswap! *state simple-move move/cursor-move-doc-start*)
     :move-doc-end
     (vswap! *state simple-move move/cursor-move-doc-end*)
     nil))
  )
