(ns chic.text-editor.cursor
  (:require
   [chic.text-editor :as text-editor :refer [PTextEditor_Pos]]
   [chic.text-editor.line :as line]
   [clojure.string :as str]
   [chic.key :as key]
   [chic.focus :as focus]
   [chic.ui.focusable :as focusable]
   [chic.keybindings :as keybindings]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [nrepl.cmdline :as nrepl]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui])
  (:import
   (chic.text_editor TextEditor)
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window Key KeyModifier]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode TextLine]
   [io.github.humbleui.types IPoint]))

(defn cursors-indexed [sm]
  (eduction (map-indexed (fn [i c]
                           (assoc c :i i)))
            (:cursors sm)))

(defn move-cursors-to-line-idx [sm line-idx-f]
  (reduce (fn [sm {:keys [idx line-id i]}]
            (let [line-order (:line-order sm)
                  line (get-in sm [:lines-by-id line-id])]
              (if-let [new-idx (line-idx-f line idx)]
                (update-in sm [:cursors i]
                           assoc :idx new-idx
                           :line-id line-id)
                sm)))
          sm
          (cursors-indexed sm)))

(defn -move-cursor-to-line-idx [sm line-idx-f forwards-or-backwards]
  (reduce (fn [sm {:keys [idx line-id i]}]
            (let [line-order (:line-order sm)]
              (loop [order-idx (line/line-order-idx sm line-id)
                     idx idx
                     line-id line-id
                     line (get-in sm [:lines-by-id line-id])]
                (if-let [new-idx (line-idx-f line idx)]
                  (update-in sm [:cursors i]
                             assoc :idx new-idx
                             :line-id line-id)
                  (let [next-order-idx (case forwards-or-backwards
                                         :forwards (inc order-idx)
                                         :backwards (dec order-idx))]
                    (if-let [next-line-id (nth line-order next-order-idx nil)]
                     (let [next-line (get-in sm [:lines-by-id next-line-id])]
                       (recur next-order-idx
                              (case forwards-or-backwards
                                :forwards -1
                                :backwards (inc (line/end-idx next-line)))
                              next-line-id
                              next-line))
                     sm))))))
          sm
          (cursors-indexed sm)))

(defn move-cursor-to-prev-line-idx [sm line-idx-f]
  (-move-cursor-to-line-idx sm line-idx-f :backwards))

(defn move-cursor-to-next-line-idx [sm line-idx-f]
  (-move-cursor-to-line-idx sm line-idx-f :forwards))

#_(extend-type TextEditor
  PTextEditor_Pos
  (at-beginning? [{:keys [state]}]
    (zero? (:pos @state)))
  (at-end? [{:keys [state] :as self}]
    (let [state' @state]
      (== (:pos state') (cond-> (count (:content state'))
                          (not (text-editor/insert-mode? self))
                          unchecked-dec))))
  (line-start-pos [{:keys [state]}]
    (line-start-pos* state))
  (line-end-pos [{:keys [state]}]
    (line-end-pos* state)))
