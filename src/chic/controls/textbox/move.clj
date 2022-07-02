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
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)))

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

(defn cursor-move-down* [{:keys [cursor-dx ^Rope rope line-start-idxs
                                 cursor-line-idx] :as state}]
  (let [next-line-idx (inc cursor-line-idx)
        has-next? (< next-line-idx (count line-start-idxs))]
    (-> state
        (cond-> has-next?
          (-> (assoc :cursor-line-idx next-line-idx)
              (as-> state (assoc state :cursor-idx (hpr/dx->cursor-idx state cursor-dx)))))
        (cond-> (not has-next?)
          (assoc :cursor-idx (.size rope)))
        (as-> state (assoc state :cursor-dx (hpr/calc-cursor-dx state))))))

(defn cursor-move-up* [{:keys [cursor-dx ^Rope rope line-start-idxs
                               cursor-line-idx] :as state}]
  (let [next-line-idx (dec cursor-line-idx)
        has-prev? (<= 0 next-line-idx)]
    (-> state
        (cond-> has-prev?
          (-> (assoc :cursor-line-idx next-line-idx)
              (as-> state (assoc state :cursor-idx (hpr/dx->cursor-idx state cursor-dx)))))
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
    :move-down
    (vswap! *state cursor-move-down*)
    :move-up
    (vswap! *state cursor-move-up*)
    :move-start
    (vswap! *state cursor-move-start*)
    :move-end
    (vswap! *state cursor-move-end*)
    :move-doc-start
    (vswap! *state cursor-move-doc-start*)
    :move-doc-end
    (vswap! *state cursor-move-doc-end*)
    nil))
