(ns chic.mccs.ui-basic
  (:require
   [clojure.core.matrix :as matrix]
   [chic.paint :as cpaint]
   [clojure.string :as str]
   [clojure.java.shell :as sh]
   [chic.util :as util]
   [chic.ui.event :as uievt]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.error :as cui.error]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter]
   [io.github.humbleui.types IPoint Point Rect RRect]
   (io.github.humbleui.jwm Screen App)
   (oshi SystemInfo)
   (oshi.util EdidUtil)
   #_(de.pitkley.jmccs.monitor MonitorManager Monitor)))

#_(grid-by-rows
   {:row-fn (fn [idx {:keys []}]
              [])})
(defn basic-view []
  (cuilay/padding
   5
   (let [screens (App/getScreens)
         max-scale (apply max (map #(.getScale %) screens))]
     (cuilay/halign
      0.5
      (cuilay/valign
       0.5
       (cuilay/stack
        (for [[idx screen] (map-indexed (fn [idx s] [idx s]) screens)]
          ;; (cui/label (str (.getBounds screen)))
          (let [bounds (.getBounds screen)
                sf0 20
                sf (/ max-scale (.getScale screen) sf0)
                *brightness (atom (some->
                                   (second (re-find #"current: (\d+)"
                                                    (:out (sh/sh "ddcctl" "-d" (str (inc idx)) "-b" "?"))))
                                   (parse-double)
                                   (/ 100)))
                available? (boolean @*brightness)]
            (cuilay/halign
             0
             (cuilay/valign
              0
              (cuilay/translate
               (/ (:x bounds) sf0) (/ (:y bounds) sf0)
               (cuilay/width
                (* (:width bounds) sf)
                (cuilay/height
                 (* (:height bounds) sf)
                 (cui/dynamic
                  ctx [{:keys [scale]} ctx]
                  (cuilay/stack
                   (if available?
                     (let [*b-agt (agent @*brightness :error-mode :continue)]
                       (cuilay/scrollable
                        (fn [{:hui.event.mouse-scroll/keys [dy] :as evt}]
                          (when (cui/point-in-component? evt (:chic.ui/mouse-win-pos evt))
                            (swap! *brightness
                                   (fn [b]
                                     (max 0. (min 1. (+ b (/ dy scale 2000))))))
                            (send *b-agt (fn [b1]
                                           (let [b2 @*brightness]
                                             (when-not (== b1 b2)
                                               (sh/sh "ddcctl" "-d" (str (inc idx)) "-b" (str (* 100 b2))))
                                             b2)))))
                        (ui/fill
                         (huipaint/fill 0xffd09000)
                         (cuilay/valign
                          1
                          (ui/fill
                           (huipaint/fill 0xfffff000)
                           (cuilay/height
                            #(* @*brightness (:height %))
                            (ui/gap 0 0)))))))
                     (ui/fill (huipaint/fill 0xff404040)
                              (ui/gap 0 0)))
                   (cuilay/padding
                    1
                    (ui/fill (huipaint/stroke 0xff000000 (* scale 2))
                             (ui/gap 0 0)))))))))))
          #_(cui/on-draw
             (fn [_ _ ^Canvas canvas]
               (.drawRect (huipaint/fill 0xff000000)))
             (ui/gap 0 0)))))))))

(comment
  (map #(.getBounds %) (App/getScreens))
  (.isPrimary (first (App/getScreens)))
  (.getScale (first (App/getScreens)))

  (sh/sh "ddcctl" "-d" "1" "-b" "40")
  (sh/sh "ddcctl" "-d" "2" "-b" "60")
  (EdidUtil/getVcm
   (.getEdid
    (first
     (.getDisplays
      (.getHardware (SystemInfo.))))))

  (map
   #(EdidUtil/getDescriptorText %)
   (EdidUtil/getDescriptors
    (.getEdid
     (first
      (.getDisplays
       (.getHardware (SystemInfo.)))))))

  (EdidUtil/getDescriptorText
   (some
    #(when (== 0xfc (EdidUtil/getDescriptorType %)) %)
    (EdidUtil/getDescriptors
     (.getEdid
      (second
       (.getDisplays
        (.getHardware (SystemInfo.))))))))

  ;; (let [mm (MonitorManager/get)
  ;;       ms (.getMonitors mm)]
  ;;   ms)
  ;; (import '(de.pitkley.jmccs.osx CoreGraphics LibMCCS OSXMonitor OSXMonitorManager))
  (filter (fn [s] (str/starts-with? s "D: "))
          (str/split-lines (:out (sh/sh "ddcctl"))))

  #!
  )

;; Unfortunately, you cannot achieve perfect pixel-alignment on macOS because all software, includint the macOS shell, draws to the inflated virtual framebuffer (which is then downsampled to the actual screen resolution). The scaling factor is 2 for high DPI displays.
;; https://en.wikipedia.org/wiki/Resolution_independence

;; I think Windows apps are able to handle fractional scaling better via DPI awareness
