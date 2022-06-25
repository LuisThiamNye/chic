(ns chic.controls.textbox.core
  (:require
   [chic.controls.textbox.cursor :as cursor]
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas)
   (io.github.humbleui.types Rect Point RRect)))

(defn textbox [])

(def scale 2)
#_(def scale 1)

#_(defn cursor-layer [{:keys [line-spacing]}]
  (ui2/receive-data
   [:origin-x :cursor-x]
   (ui2/translating
    (fn [ctx])
    (ui2/sized-with
     (fn [_] (Point. (* 2. scale) line-spacing))
     (ui2/fill-rect (huipaint/fill 0xE0007ACC))))))

(defn textbox-sample []
  (let [font (Font. style/face-code-default (* scale 14.))
        line-spacing (.getSpacing font)
        *state (volatile! {:rope (.insert Rope/EMPTY 0 "Hello there.\nIt is me")
                           :cursor-idx 0
                           :cursor-dx 0
                           :select-idx nil
                           :cursor-line-idx 0
                           :line-start-idxs []
                           :line-end-idxs []
                           :layout {:line-height line-spacing
                                    :first-line-origin 'Point
                                    :first-line-x 0
                                    :text-plane-rect (Rect. 0. 0. 0. 0.)
                                    :line-end-xs []}})]
    (ui2/stack
     (ui2/padded 0.5 (ui2/fill-rect (huipaint/stroke 0x5f000000 1)))
     (ui2/padded
      (* scale 4)
      (ui2/watch-rect
       (fn --f1 [_ rect]
         (vswap! *state (fn [state]
                          (-> state
                              (assoc-in [:layout :text-plane-rect] rect)))))
       (ui2/stack
        (ui2/eqicolumn*
         line-spacing
         (into []
               (comp
                (partition-by
                 (fn [c] (= 10 c)))
                (remove #{[10]})
                (map (fn [chs]
                       (let [s (apply str (map char chs))]
                         (ui2/sized-with
                          (fn [_] (Point. 400. line-spacing))
                          #_(ui2/ph-textline s {})
                          (ui2/padded [0 0 0 (.getDescent (.getMetrics font))]
                                      (ui2/text-string s font)))))))
               (iterator-seq (.chars (:rope @*state)))))
        #_(cursor-layer {:line-spacing line-spacing})
        (cursor/cursor-w
         {:getters {:paint (constantly (huipaint/fill 0xE0007ACC))
                    :line-height (fn [_] (:line-height (:layout @*state)))
                    :line-top (fn [_] (let [state @*state
                                            layout (:layout state)]
                                        (+ (:y (:text-plane-rect layout))
                                           (* (:cursor-line-idx state) (:line-height layout)))))
                    :cursor-x (fn [_] (+ (:x (:text-plane-rect (:layout @*state)))
                                         (:cursor-dx @*state)))}})
        #_((ui2/direct-widget
            {:draw (cursor/selction-draw-fn-for-layout
                    {:line-height 30.
                     :first-line-origin (Point. 105. 40.)
                     :first-line-x 300.
                     :line-end-xs [500. 300. 311.]}
                    (huipaint/fill 0xa0007ACC))}) {})))))))

(comment
  (chic.windows/remount-all-windows)
  (alter-var-root #'scale (constantly 1))
  (alter-var-root #'scale (constantly 2))

  #!
  )
;; short term goal: clojure notepad. Create small documents of clojure code.
;; code data can be accessed programmatically and also evaluated into vars
