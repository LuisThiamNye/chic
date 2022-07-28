(ns chic.uiroot
  (:require
   [chic.debug.debugger :as debugger]
   [chic.demo :as demo]
   [chic.focus :as focus]
   [chic.style :as style]
   [chic.ui :as cui]
   [chic.ui.event :as uievt]
   [chic.ui.layout :as cuilay]
   [chic.windows :as windows]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin])
  (:import
    (io.github.humbleui.jwm App Screen)
    (io.github.humbleui.skija Font Paint)))

(def *pressed-keys (volatile! #{}))
(def focus-manager (focus/new-manager))

(def key-indicator
  (ui/dynamic
    ctx [pressed-keys @*pressed-keys
         font-ui (:font-ui ctx)
         fill-text (:fill-text ctx)]
    (let [pressed-keys pressed-keys
          font-ui font-ui
          fill-text fill-text]
      (ui/on-key-down
        #(vswap! *pressed-keys conj (:hui.event.key/key %))
        (ui/on-key-up
          #(vswap! *pressed-keys disj (:hui.event.key/key %))
          (let [s (apply str
                    (eduction
                      (remove nil?)
                      [(when (contains? pressed-keys "Ctrl")
                         "C-")
                       (when (or (contains? pressed-keys "Option")
                               (contains? pressed-keys "Alt"))
                         "A-")
                       (when (contains? pressed-keys "Command")
                         "H-")
                       (when (contains? pressed-keys "Shift")
                         "S-")
                       (when-let [letters (seq (eduction (filter #(== 1 (count %))) pressed-keys))]
                         (apply str letters))]))]
            (ui/label s font-ui fill-text)))))))

(def *app-root (volatile! nil))

(def *scale2 (volatile! nil))

(defn build-app-root []
  (ui/dynamic
    ctx [scale (:scale ctx)]
    (let [font-ui (Font. style/face-default (float (* 14 scale)))
          leading (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          font-code (Font. style/face-code-default (float (* 14 scale)))]
      (ui/with-context {:face-ui style/face-default
                        :face-code style/face-code-default
                        :font-code font-code
                        :font-ui font-ui
                        :focus-manager focus-manager
                        :leading leading
                        :fill-text fill-text}
        (cuilay/column
          [:stretch 1
           (cui/updating-ctx
             (fn [ctx] (if-some [s @*scale2]
                         (assoc ctx :scale s)
                         ctx))
             (cui/dyncomp
               (demo/basic-view)))]
          #_[:stretch 1
             (ui/gap 0 0)]
          (cuilay/height
            20
            (ui/fill
              (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
              (cuilay/row
                key-indicator
                [:stretch 1
                 (ui/gap 0 0)]
                (cui/clickable
                  (uievt/on-primary-down
                    (fn [_evt] (cui/refresh-all-dyncomps!)))
                  (ui/fill (huipaint/fill 0x11000000)
                    (cuilay/valign
                      0.5 (cuilay/padding 20 0 (ui/label "Refresh" font-ui fill-text)))))
                (ui/gap 3 0)
                (cui/clickable
                  (fn [event]
                    (when (:hui.event.mouse-button/is-pressed event)
                      (vswap! windows/*always-render? not)))
                  (ui/fill (huipaint/fill 0x11000000)
                    (cuilay/valign
                      0.5 (cuilay/padding 10 0 (ui/label "60" font-ui fill-text)))))
                (cui/clickable
                  (uievt/on-primary-down (fn [_] (debugger/show-error-window)))
                  (ui/dynamic
                    _ctx [error-count (count (:error-stack @debugger/*state))]
                    (ui/fill (huipaint/fill (if (pos? error-count)
                                              0x20503000
                                              0x09000000))
                      (cuilay/valign
                        0.5 (cuilay/padding 10 0 (ui/label (str error-count " E") font-ui fill-text))))))
                (ui/contextual
                  (fn [{:keys [:chic.profiling/time-since-last-paint :chic/current-window]}]
                    (let [{:keys [latest-paint-duration]} @(:*profiling current-window)
                          millis (/ latest-paint-duration 1000000.)]
                      (ui/fill
                        (huipaint/fill (if (< 16000000 latest-paint-duration)
                                         0xFFFF0000
                                         0x00FFFFFF))
                        (cuilay/width
                          150
                          (cuilay/halign
                            1 (cuilay/padding
                                5 0 (cuilay/valign
                                      0.5 (ui/label (format "%3d fps %6.3f ms"
                                                      (unchecked-int (/ 1000000000 time-since-last-paint))
                                                      millis)
                                            font-code fill-text)))))))))
                (cui/clickable
                  (fn [event]
                    (when (:hui.event.mouse-button/is-pressed event)
                      (windows/remount-all-windows)))
                  (ui/fill (huipaint/fill 0x11000000)
                    (cuilay/valign
                      0.5 (cuilay/padding 20 0 (ui/label "Reload" font-ui fill-text)))))))))))))

(defn make-main-window []
  (let [screen ^Screen (last (App/getScreens))
        scale (.getScale screen)
        width (* 600 scale)
        height (* 400 scale)
        area (.getWorkArea screen)
        x (:x area)
        y (-> (:height area) (- height))
        _ (prn y)
        w (windows/make2
            {:id "main"
             :*app-root *app-root
             :build-app-root #(cui/dyncomp (build-app-root))
             :on-close #(fn [])})]
    (doto (:window-obj w)
      (huiwin/set-title "Main")
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y))
    (windows/set-visible w true)))

(defn start-ui []
  (debugger/install-debug-ctx!)
  (App/start make-main-window))

