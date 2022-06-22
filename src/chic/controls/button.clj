(ns chic.controls.button
  (:require
   [taoensso.encore :as enc]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.ui.event :as uievt]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [criterium.core :as crit])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter Font]
   [io.github.humbleui.types IPoint Point Rect RRect]))

(defn lone-button [{:keys [label on-click]}]
  {:pre [(some? label)]}
  (let [font-size 13
        blur 2.
        on-click (or on-click (fn [_]))]
    (cui/with-bounds
      :button-size
      (cui/dynamic
        ctx [{:keys [scale]} ctx
             button-height (* scale (:height (:button-size ctx)))]
       (let [unpressed-fg-gradient
             (Shader/makeLinearGradient
              0. 1. 0. (float (dec button-height))
              (int-array [(cpaint/grey 255) (cpaint/grey 251)]))
             fg-paint (doto (Paint.)
                        (.setShader unpressed-fg-gradient))]
         (cui/dyncomp
          (cui/standard-button
           {:on-click on-click
            :no-hover? true
            :on-change (fn [_ state]
                         (case state
                           :pressed
                           (.setShader fg-paint
                                       (Shader/makeLinearGradient
                                        0. 1. 0. (float (dec button-height))
                                        (int-array [(cpaint/grey 240) (cpaint/grey 245)])))
                           :unpressed
                           (.setShader fg-paint unpressed-fg-gradient)
                           nil))}
           (ui/clip-rrect
            6
            (ui/fill
             (doto (Paint.)
               (.setShader (Shader/makeLinearGradient
                            0. 0. 0. (float button-height)
                            (int-array [(cpaint/grey 220) (cpaint/grey 190)]))))
             (cuilay/padding
              1
              (ui/clip-rrect
               5
               (ui/fill
                fg-paint
                (let [f (fn [label-text]
                          (cui/dynamic
                            ctx [{:keys [fill-text font-ui scale]} ctx]
                            (cui/label label-text (.makeWithSize ^Font font-ui (* scale font-size)) fill-text)))]
                  (cuilay/padding
                   6 4
                   (if (ifn? label)
                     (cui/dynamic
                       ctx [label-text (label ctx)]
                       (f label-text))
                     (f label))))))))))))))))
