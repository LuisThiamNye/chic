(ns chic.controls-demo
  (:require
   [chic.ui.ui2 :as ui2]
   [chic.controls.textbox.core :as textbox]
   [clojure.core.matrix :as matrix]
   [taoensso.encore :as enc]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.controls.checkbox :as checkbox]
   [chic.controls.button :as button]
   [chic.ui.event :as uievt]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.controls.combo :as combo]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter]
   [io.github.humbleui.types IPoint Point Rect RRect]))

(defn basic-view []
  (cuilay/padding
   5
   (cuilay/column
    (let [*checkstate (volatile! false)]
      (checkbox/ui-checkbox
       {:label (fn [_] (str "Checkbox is " (case @*checkstate
                                             true "checked"
                                             false "unchecked"
                                             (name @*checkstate))))
        :state-fn (fn [_] @*checkstate)
        :on-toggle (fn [checked?]
                     (vreset! *checkstate (if checked? :checking :unchecking))
                     (enc/after-timeout 500
                                        (vreset! *checkstate checked?)))}))
    (ui/gap 0 8)
    (let [*checked (volatile! false)]
      (cui/dyncomp
       (checkbox/ui-checkbox {:size :small
                              :state-fn (fn [_] @*checked)
                              :label "Small with static label"
                              :on-toggle (fn [checked?] (vreset! *checked checked?))})))
    (ui/gap 0 8)
    (cuilay/halign
     0 (let [*count (volatile! 0)]
       (cui/dyncomp
        (button/lone-button
         {:label (fn [_] (str "Click Me: " @*count))
          :on-click (fn [_] (vswap! *count inc))}))))
    (ui/gap 0 8)
    (cuilay/halign
     0 (combo/combo-box {}))
    [:stretch 1 (ui/gap 0 0)])))

(defn textbox-demo []
  (ui2/v1-root
   {}
   (ui2/clip-rect
    (ui2/padded
     8
     (textbox/textbox-sample)))))
