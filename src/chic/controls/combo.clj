(ns chic.controls.combo
  (:require
   [taoensso.encore :as enc]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.ui.event :as uievt]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [chic.style :as style]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.window :as huiwin]
   [chic.windows :as windows]
   [io.github.humbleui.ui :as ui]
   [criterium.core :as crit])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter Font]
   [io.github.humbleui.types IPoint Point Rect RRect]))

(defn build-menu-window-root [{}]
  (cui/on-event
   (fn [evt]
     (when (= :hui/window-focus-out (:hui/event evt))
       (huiwin/close (:window-obj (:chic/current-window evt)))
       false))
   (ui/dynamic
     ctx [{:keys [scale]} ctx]
     (ui/with-context
       {:font-ui (Font. style/face-code-default (float (* scale 12)))
        :fill-text (huipaint/fill 0xFF000000)
        :font-code (Font. style/face-code-default (float (* 12 scale)))}
       (ui/dynamic
         ctx [{:keys [fill-text font-ui]} ctx]
         (ui/fill
          (huipaint/fill 0xFF000000)
          (chic.controls.button/lone-button {:label "button"})
          #_(ui/gap 0 0)))))))

(defn make-menu-window [{}]
  (let [screen (last (hui/screens))
        scale (:scale screen)
        area (:work-area screen)
        width (* 600 scale)
        height (int (* 0.9 (:height area)))
        x (-> (:width area) (- width) (/ 2) (+ (:x area)))
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
      (windows/make
       {:id (random-uuid)
        :build-app-root (fn [] (cui/dyncomp (build-menu-window-root {})))
        :on-close (fn [])})
      (huiwin/set-title (str "Context Menu"))
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-z-order :floating)
      (.setTitlebarVisible false)
      (huiwin/set-visible true)
      (.focus))))

(defn combo-box [{}]
  (cui/standard-button
   {:on-click (fn [_] (windows/dosendui
                       (make-menu-window {})))}
   (cui/label "Combo")))

(comment
  (windows/dosendui
   (make-menu-window {}))
  (def --win (:window-obj (second (vals @windows/*windows))))
  (windows/dosendui (.close --win))
  (windows/dosendui (.focus --win))
  (enc/after-timeout
   1000
   (prn )4
   (windows/dosendui
    (.focus (:window-obj (first (vals @windows/*windows))))))

  (windows/dosendui
   (.setTitlebarVisible --win false))
  (windows/dosendui
   (huiwin/set-z-order --win :modal-panel))
  #!
  )
