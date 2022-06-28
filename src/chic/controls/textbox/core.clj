(ns chic.controls.textbox.core
  (:require
   [chic.controls.textbox.cursor :as cursor]
   [chic.style :as style]
   [chic.ui2.event :as ievt]
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
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)))

(def scale 2)
#_(def scale 1)

(defn textbox [])

(defn textbox-sample []
  (let [font (Font. style/face-code-default (* scale 14.))
        line-spacing (.getSpacing font)
        init-rope (.insert Rope/EMPTY 0 "Hello there.\nIt is me")
        *state (volatile! {:rope init-rope
                           :cursor-idx 0
                           :cursor-dx 0
                           :select-idx nil
                           :cursor-line-idx 0
                           :line-start-idxs [0 13]
                           :line-end-idxs []
                           :text-lines (into []
                                             (comp
                                              (partition-by
                                               (fn [c] (= 10 c)))
                                              (remove #{[10]})
                                              (map (fn [chs]
                                                     (let [s (apply str (map char chs))]
                                                       (uifont/shape-line-default font s)))))
                                             (iterator-seq (.chars ^Rope init-rope)))
                           :layout {:line-height line-spacing
                                    :first-line-origin (Point. 0. 0.)
                                    :first-line-x 0
                                    :text-plane-rect (Rect. 0. 0. 0. 0.)
                                    :line-end-xs []}})
        cursor-paint (huipaint/fill 0xE0007ACC)
        border-paint (huipaint/stroke 0x5f000000 1)]
    (ui2/attach-interactor
     {:cursor-style :ibeam
      :on-mouse-down
      (fn [ctx rect evt]
        (ievt/case-mousebtn evt
          :primary
          (let [{:keys [line-start-idxs text-lines]
                 {:keys [first-line-origin]} :layout} @*state
                {:keys [x y]} (ui2/get-mouse-pos ctx)
                dy (- y (:y first-line-origin))
                lidx (long (Math/floor (/ dy line-spacing)))
                nlines (count line-start-idxs)
                lidx (max 0 (min (dec nlines) lidx))
                text-line ^TextLine (nth text-lines lidx)
                dx (- x (:x first-line-origin))
                cidx (.getOffsetAtCoord text-line dx)]
            (vswap! *state (fn [state]
                             (-> state
                                 (assoc :cursor-dx (.getCoordAtOffset text-line cidx))
                                 (assoc :cursor-line-idx lidx)))))))
      :on-mouse-up (fn [evt])
      :on-scroll (fn [evt])}
     (ui2/stack
      (ui2/padded 0.5 (ui2/fill-rrect (* scale 2) border-paint))
      (ui2/padded
       (* scale 4)
       (ui2/watch-rect
        (fn --f1 [_ rect]
          (vswap! *state (fn [state]
                           (-> state
                               (assoc-in [:layout :first-line-origin] (Point. (:x rect) (:y rect)))
                               (assoc-in [:layout :text-plane-rect] rect)))))
        (ui2/stack
         (ui2/eqicolumn*
          line-spacing
          (mapv (fn [text-line]
                  (ui2/sized-with
                   (fn [_] (Point. 400. line-spacing))
                   (ui2/padded [0 0 0 (.getDescent (.getMetrics font))]
                               (ui2/textline text-line))))
                (:text-lines @*state)))
         #_(cursor-layer {:line-spacing line-spacing})
         (cursor/cursor-w
          {:getters {:paint (constantly cursor-paint)
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
                     (huipaint/fill 0xa0007ACC))}) {}))))))))

(comment
  (chic.windows/remount-all-windows)
  (alter-var-root #'scale (constantly 1))
  (alter-var-root #'scale (constantly 2))

  ;; interactor
  ;; mouse areas

#!
  )
;; short term goal: clojure notepad. Create small documents of clojure code.
;; code data can be accessed programmatically and also evaluated into vars
