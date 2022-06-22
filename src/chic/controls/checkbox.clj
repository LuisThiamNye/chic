(ns chic.controls.checkbox
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
   [io.github.humbleui.skija Paint Shader Canvas PaintMode Path PaintStrokeCap Matrix33 ImageFilter]
   [io.github.humbleui.types IPoint Point Rect RRect]))

;; Recommended lengths: 15->regular 12->small
(defn make-checkbox-only-draw-fn [scale length ^"[I" outer-colours ^"[I" inner-colours
                                  checkmark? check-colour]
  (let [thickness (float (math/round (* 1. scale (/ length 15))))
        stroke-thickness (* scale 2 (/ length 15))
        size (float (math/round (* length scale)))
        brad (Math/ceil (* (/ length 3.75) scale))
        outer-rrect (RRect/makeLTRB 0 0 size size brad)
        inner-rrect (RRect/makeLTRB thickness thickness (- size thickness) (- size thickness) (- brad thickness))
        blur (/ size 10)
        shadow-dy (/ size 30)
        shadow-paint (doto (Paint.)
                       (.setImageFilter
                        (ImageFilter/makeDropShadowOnly
                         0. shadow-dy (/ blur 2) (/ blur 2) (unchecked-int 0x603984f0))))
        outer-paint (doto (Paint.)
                      (.setShader (Shader/makeLinearGradient
                                   0. thickness 0. size
                                   outer-colours)))
        inner-paint (doto (Paint.)
                      (.setShader (Shader/makeLinearGradient
                                   0. thickness 0. (- size thickness)
                                   inner-colours)))]
    (if checkmark?
      (let [check-path (doto (Path.)
                         (.moveTo 3.2 8.)
                         (.tangentArcTo 6.2 11.5 11.2 4. (* 0.02 stroke-thickness))
                         (.lineTo 11.2 4.)
                         (.transform (Matrix33/makeScale (* scale (/ length 15)))))
            check-stroke (doto (huipaint/stroke check-colour stroke-thickness)
                           (.setStrokeCap PaintStrokeCap/ROUND)
                           (.setStrokeMiter 1.))]
        (fn [_ctx _cs ^Canvas canvas]
          (.drawRRect canvas outer-rrect shadow-paint)
          (.drawRRect canvas outer-rrect outer-paint)
          (.drawRRect canvas inner-rrect inner-paint)
          (.drawPath canvas check-path check-stroke)))
      (fn [_ctx _cs ^Canvas canvas]
        (.drawRRect canvas outer-rrect shadow-paint)
        (.drawRRect canvas outer-rrect outer-paint)
        (.drawRRect canvas inner-rrect inner-paint)))))

(defn make-checkedbox-only-draw-fn [scale length]
  (make-checkbox-only-draw-fn
   scale
   length
   (int-array [0xff4f92f5 0xff1269db])
   (int-array [0xff3984f0 0xff1470e8])
   true
   0xffffffff))

(defn make-checkedbox-only-pressed-draw-fn [scale length]
  (make-checkbox-only-draw-fn
   scale
   length
   (int-array [0xff4f92f5 0xff1269db])
   (int-array [0xff3984f0 0xff1470e8])
   true
   0xffffffff))

(defn make-uncheckedbox-only-draw-fn [scale length]
  (make-checkbox-only-draw-fn
   scale
   length
   (int-array [(cpaint/grey 0xd2) (cpaint/grey 0xc9)])
   (int-array [(cpaint/grey 0xf3) (cpaint/grey 0xff)])
   false
   nil))

(defn ui-checkbox-only [length state-fn]
  (cuilay/padding
   1 1 1 2
   (cui/dynamic ctx [{:keys [scale]} ctx
                     state (state-fn ctx)]
                (cui/on-draw
                 (case state
                   false (make-uncheckedbox-only-draw-fn scale length)
                   true (make-checkedbox-only-draw-fn scale length)
                   :disabled (make-uncheckedbox-only-draw-fn scale length)
                   :disabled-checked (make-uncheckedbox-only-draw-fn scale length)
                   :unchecking (make-checkedbox-only-draw-fn scale length)
                   :checking (make-uncheckedbox-only-draw-fn scale length))
                 (ui/gap length length)))))

(defn checkbox-geometry [size label state-fn]
  (let [font-size (case size
                    (:regular nil) 13
                    :small 11)
        length (case size
                 (:regular nil) 15
                 :small 12)]
    (if label
      (cuilay/row
       (cui/dyncomp
        (ui-checkbox-only length state-fn))
       (ui/gap 3 0)
       (cuilay/valign
        0.5
        (let [f (fn [label-text]
                  (cui/dynamic
                   ctx [{:keys [fill-text font-ui scale]} ctx]
                   (cui/label label-text (.makeWithSize font-ui (* scale font-size)) fill-text)))]
          (if (ifn? label)
            (cui/dynamic
             ctx [label-text (label ctx)]
             (f label-text))
            (f label)))))
      (cui/dyncomp
       (ui-checkbox-only length state-fn)))))

;; recommended font sizes: 13->regular 11->small
(defn ui-checkbox [{:keys [size label on-toggle state-fn]}]
  (let [*state (volatile! false)
        state-fn (or state-fn (fn [_] :disabled))
        on-toggle (or on-toggle (fn [_]))]
    (cui/on-draw
     (fn [ctx _ _]
       (vreset! *state (state-fn ctx)))
     #_(cuii/interactable
        [(cuii/standard-button
          {:no-hover? true
           :on-click (fn []
                       (case @*state
                         true (on-toggle false)
                         false (on-toggle true)
                         nil))})])
     (cui/standard-button
      {:on-click
       (fn [evt]
         (case @*state
           true (on-toggle false)
           false (on-toggle true)
           nil))}
      (checkbox-geometry size label (fn [_] @*state))))))

#_(let [rects (mapv (fn [_] (Rect/makeXYWH (rand 300) (rand 400) (rand 300) (rand 400))) (range 2000))
        p (Point. 50 50)
        x (:x p)
        y (:y p)]
    (criterium.core/quick-bench
     (loop [rects rects
            unders []
            i (dec (count rects))]
       (if (neg? i)
         (count unders)
         (let [rect (nth rects i)]
           (recur
            rects
            (if (and (<= (:x rect) x)
                     (<= (:y rect) y)
                     (<= x (:right rect))
                     (<= y (:bottom rect)))
              (conj unders rect)
              unders)
            (dec i)))))))
