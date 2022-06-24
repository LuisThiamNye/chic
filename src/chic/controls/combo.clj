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
   [io.github.humbleui.types IPoint Point Rect RRect]
   [io.github.humbleui.jwm Window]))

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

(defn make-menu-window [{:keys [window mouse-pos]}]
  {:pre [(some? mouse-pos) (map? window)]}
  (let [screen (.getScreen ^Window (:window-obj window))
        scale (.getScale screen)
        area (.getWorkArea screen)
        width (* 100 scale)
        height (* 300 scale)
        window-rect (huiwin/window-rect (:window-obj window))
        x (+ (:x window-rect) (:x mouse-pos))
        y (+ (:y window-rect) (:y mouse-pos))
        w (windows/make2
           {:id (random-uuid)
            :build-app-root (fn [] (cui/dyncomp (build-menu-window-root {})))
            :on-close (fn [])})
        wo (:window-obj w)]
    (doto wo
      (huiwin/set-title (str "Context Menu"))
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-z-order :pop-up-menu #_:floating)
      (.setTitlebarVisible false))
    ;; (huiwin/set-visible wo true)
    (windows/set-visible w true)
    (.focus wo)))

(defn combo-box [{}]
  (cui/standard-button
   {:on-click (fn [evt] (windows/safe-dosendui
                       (make-menu-window {:window (:chic/current-window evt)
                                          :mouse-pos (:chic.ui/mouse-win-pos evt)})))}
   (cui/label "Combo")))

(comment
  (windows/dosendui
   (try (make-menu-window {:window (first (vals @windows/*windows))
                       :mouse-pos {:x 0 :y 0}})
        (catch Exception e
          (chic.debug/println-main e))))
  (windows/window-app-rect (:window-obj (first (vals @windows/*windows))))
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
