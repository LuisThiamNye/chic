(ns chic.text-editor.move
  (:require
   [chic.text-editor :as text-editor :refer [PTextEditor_Element PTextEditor_Move]]
   [chic.text-editor.cursor :as cursor]
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
   (chic.text_editor.element TextLineSegment)
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window Key KeyModifier]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode TextLine]
   [io.github.humbleui.types IPoint]))

#_(defn closest-idx-to-x [{:keys [face-default]} s x]
    (let [font (Font. face-default (float 13))
          line-length (.getWidth (TextLine/make s font))
          guess (int (* (dec (count s)) (min 1 (/ x line-length))))]
      guess))

(comment
  #_(closest-idx-to-x @(:state chic.main/editor) "abcde" 1000)

  #!
  )

(defn move-up-or-down [sm up?]
  (reduce
   (fn [sm {:keys [idx i line-id] :as cursor}]
     (if-let [next-line-id ((if up? line/prev-line line/next-line) sm line-id)]
       (let [segs (get-in sm [:lines-by-id line-id :ui-segments])
             x (loop [seg-i 0
                      width-acc 0]
                 (let [seg (nth segs seg-i)]
                   (let [seg-width (:x (:size (:ui seg)))]
                     (if (= i (:cursor-id seg))
                       (if (:insert-mode? sm)
                         width-acc
                         (unchecked-add-int width-acc (int (/ seg-width 2))))
                       (recur (unchecked-inc-int seg-i)
                              (unchecked-add-int width-acc seg-width))))))
             next-line (get-in sm [:lines-by-id next-line-id])
             next-segs (:ui-segments next-line)
             text-seg? (fn [seg] (instance? TextLineSegment (:ui seg)))
             [target-seg target-seg-offset]
             (loop [seg-i 0
                    width-acc 0
                    prev-text-seg-and-bounds nil]
               (if-let [seg (nth next-segs seg-i nil)]
                 (let [seg-width (:x (:size (:ui seg)))
                       next-width (unchecked-add-int width-acc seg-width)]
                   (if (< next-width x)
                     (recur (unchecked-inc-int seg-i)
                            (unchecked-add-int width-acc seg-width)
                            (if (text-seg? seg)
                              [seg width-acc next-width]
                              prev-text-seg-and-bounds))
                     (if (text-seg? seg)
                       [seg width-acc]
                       (loop [seg-i seg-i
                              width-acc width-acc]
                         (if-let [ahead-seg (nth next-segs seg-i nil)]
                           (if (text-seg? ahead-seg)
                             (if (> (unchecked-subtract-int width-acc x)
                                    (unchecked-subtract-int x (nth prev-text-seg-and-bounds 2)))
                               (pop prev-text-seg-and-bounds)
                               [ahead-seg width-acc])
                             (recur (unchecked-inc-int seg-i)
                                    (unchecked-add-int width-acc (:x (:size (:ui seg))))))
                           (pop prev-text-seg-and-bounds))))))
                 (pop prev-text-seg-and-bounds)))
             closest-info ((:closest-char-fn target-seg) (- x target-seg-offset))
             target-idx (unchecked-add-int
                         (:start-idx target-seg)
                         (cond-> (:local-idx closest-info)
                           (and (:insert-mode? sm) (:right-side? closest-info))
                           (-> inc (min (line/end-idx next-line)))))]
         (assoc-in sm [:cursors i] (assoc cursor
                                          :idx target-idx
                                          :line-id next-line-id)))
       sm))
   sm
   (cursor/cursors-indexed sm)))

(extend-type TextEditor
  PTextEditor_Move
  (move-up [{:keys [state]}]
    (swap! state move-up-or-down true))
  (move-down [{:keys [state]}]
    (swap! state move-up-or-down false))
  (move-forwards [{:keys [state] :as self}]
    (swap! state (fn [sm]
                   (reduce (fn [sm {:keys [idx i line-id]}]
                             (if (== idx (line/end-idx (get-in sm [:lines-by-id line-id])))
                               sm
                               (update-in sm [:cursors i :idx] inc)))
                           sm
                           (cursor/cursors-indexed sm)))))
  (move-backwards [{:keys [state] :as self}]
    (swap! state (fn [sm]
                   (reduce (fn [sm {:keys [idx i line-id]}]
                             (if (zero? idx)
                               sm
                               (update-in sm [:cursors i :idx] dec)))
                           sm
                           (cursor/cursors-indexed sm))))))