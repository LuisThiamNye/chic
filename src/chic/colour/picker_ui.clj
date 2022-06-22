(ns chic.colour.picker-ui
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
   (io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter
                             Bitmap Image)
   [io.github.humbleui.types IPoint Point Rect RRect]))

(defn fix-b [n]
  (if (> n 127)
    (- n 256)
    n))

(defn ^"[B" sv-square-colours [width height h]
  (let [ba (byte-array (* 4 width height))
        vs (mapv #(- 1 (/ % (dec height))) (range height))
        width- (dec width)]
    (dotimes [x width]
      (let [s (/ x width-)]
        (dotimes [y height]
          (let [rgb (cpaint/okhsv->srgb h s (nth vs y))
                i (* 4 (+ (* y width) x))]
            ;; N32 is BGRA_8888
            (aset-byte ba i (fix-b (Math/round (.-z rgb))))
            (aset-byte ba (+ i 1) (fix-b (Math/round (.-y rgb))))
            (aset-byte ba (+ i 2) (fix-b (Math/round (.-x rgb))))
            (aset-byte ba (+ i 3) -1)))))
    ba))
;; (sv-square-colours 100 100 0.5)

(defn ^"[B" hue-spectrum-colours [width height]
  (let [ba (byte-array (* 4 width height))
        height- (dec height)]
    (dotimes [y height]
      (let [h (/ y height-)
            rgb (cpaint/okhsv->srgb h 1. 1.)
            b (fix-b (Math/round (.-z rgb)))
            g (fix-b (Math/round (.-y rgb)))
            r (fix-b (Math/round (.-x rgb)))]
        ;; N32 is BGRA_8888
        (dotimes [x width]
          (let [i (* 4 (+ (* y width) x))]
            (aset-byte ba i b)
            (aset-byte ba (+ i 1) g)
            (aset-byte ba (+ i 2) r)
            (aset-byte ba (+ i 3) -1)))))
    ba))
;; (hue-spectrum-colours 20 200)

(defn bitmap-from-n32-ary [width height ba]
  (let [bitmap (doto (Bitmap.)
                 (.allocN32Pixels width height))]
    (.installPixels bitmap ba)
    bitmap))

#_(huipaint/fill
   (+ 0xff000000
      (bit-shift-left (+ 128 (aget colours i0)) 16)
      (bit-shift-left (+ 128 (aget colours (+ i0 1))) 16)
      (+ 128 (aget colours (+ i0 2)))))

(defn ui-hue-spectrum [width height]
  (let [image (Image/makeFromBitmap
               (.setImmutable
                (bitmap-from-n32-ary
                 width height (hue-spectrum-colours width height))))]
    (cui/on-draw
     (fn [_ _2 ^Canvas canvas]
       (.drawImage canvas image 0 0))
     (ui/gap width height))))

(defn ui-sv-square [width height]
  (let [image (Image/makeFromBitmap
               (.setImmutable
                (bitmap-from-n32-ary
                 width height (sv-square-colours width height 0.08))))]
    (cui/on-draw
    (fn [_ _2 ^Canvas canvas]
      (.drawImage canvas image 0 0))
    (ui/gap width height))))

(defn basic-view []
  (ui/fill
   (huipaint/fill (cpaint/grey 27))
   (cuilay/halign
    0.5 (cuilay/valign
         0.5 (cuilay/row
              (cuilay/stack
               (cui/dyncomp
                (ui-sv-square 200 200))
               (cui/on-draw
                (fn [{:keys [scale]} _ ^Canvas canvas]
                  (let [layer (.save canvas)]
                    (.scale canvas (float scale) (float scale))
                    (let [p (huipaint/fill (cpaint/grey 150))]
                      (.drawRect canvas (Rect/makeLTRB 7. 0. 8. 5.) p)
                      (.drawRect canvas (Rect/makeLTRB 7. 10. 8. 15.) p)
                      (.drawRect canvas (Rect/makeLTRB 0. 7. 5. 8.) p)
                      (.drawRect canvas (Rect/makeLTRB 10. 7. 15. 8.) p))
                    (.drawCircle canvas
                                7.5 7.5 5
                                (huipaint/stroke
                                 (cpaint/grey 150)
                                 1))
                    (.restoreToCount canvas layer)))
                (ui/gap 30 30)))
              (ui/gap 10 0)
              (cuilay/stack
               (cui/dyncomp
                (ui-hue-spectrum 25 200))
               (cuilay/padding
                18 50 0 0
                (cui/on-draw
                 (fn [{:keys [scale]} _ ^Canvas canvas]
                   (let [layer (.save canvas)
                         blur (* scale 5.)
                         shadow-paint (doto (Paint.)
                                        (.setImageFilter
                                         (ImageFilter/makeDropShadowOnly
                                          (* scale -0.5) 0. (/ blur 2) (/ blur 2) (unchecked-int 0xa0000000))))
                         path (doto (Path.)
                                (.moveTo 0 10)
                                (.lineTo 13 15)
                                (.lineTo 13 5)
                                (.closePath))]
                     (.scale canvas (float scale) (float scale))
                     (.drawPath canvas path shadow-paint)
                     (.drawPath canvas path (huipaint/fill 0xffffffff))
                     (.drawPath canvas path (huipaint/stroke (cpaint/grey 150) (* scale 0.5)))
                     (.restoreToCount canvas layer)))
                 (ui/gap 30 30)))))))))

;; crosshair
;; slider

(comment
  (def --bitmap (let [image (Image/makeFromEncoded (babashka.fs/read-all-bytes "22.png"))]
                  (Bitmap/makeFromImage image)))
  (.getRowBytes --bitmap)
  (.isSRGB(.getColorSpace (.getImageInfo --bitmap))) ; true
  (.getColorType (.getImageInfo --bitmap)) ; RGBA_8888
  (.getBytesPerPixel (.getImageInfo --bitmap)) ; 4
  (seq (.readPixels --bitmap))
  ; => (-1 0 0 -1   0 0 -1 -1   0 0 0 -1   0 0 0 -1)
  ;; RGBA
  (.getColor --bitmap 0 0)
  (Integer/toHexString
   (.getColor --bitmap 0 0)) ; ffff0000
  (Integer/toHexString
   (.getColor --bitmap 0 1)) ; ff000000
  (Integer/toHexString
   (.getColor --bitmap 1 0)) ; ff0000ff
  (Integer/toHexString
   (.getColor --bitmap 1 1)) ; ff000000

  (with-open [bitmap (doto (Bitmap.)
                       (.allocN32Pixels 1 1))]
    (.installPixels bitmap (byte-array [0 0 -1 -1]))
    bitmap)
  (cpaint/okhsv* )
  #!
  )
