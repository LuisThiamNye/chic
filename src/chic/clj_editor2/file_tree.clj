(ns chic.clj-editor2.file-tree
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.ui.ui2 :as ui2]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.window :as huiwin]
   [chic.paint :as cpaint]
   [chic.windows :as windows])
  (:import
   (io.github.humbleui.jwm Window)
   (io.github.humbleui.skija Paint Shader Canvas ClipMode Font ImageFilter)
   (io.github.humbleui.types Rect Point)))

;; (fs/list-dir (io/file "."))

;; upper rounded shadow
#_(let [blur (* scale 5.)
        br (float (* 0.4 (:height rect)))
        rrect (.withRadii (.withBottom rect
                                       (+ (:bottom rect) 500.))
                          br)]
    (ui2/with-save cnv
      (.clipRect cnv (Rect. (:x rect) 0. (:right rect)
                            (/ (+ (:bottom rect) (:y rect)) 2)))
      (.drawRRect cnv (.withRadii (.inflate rect 0.5) (+ br 0.5))
                  (huipaint/stroke (cpaint/grey 230) 1))
      (.clipRRect cnv rrect ClipMode/DIFFERENCE)
      (.drawRRect cnv rrect
                  (doto (Paint.)
                    (.setImageFilter
                     (ImageFilter/makeDropShadowOnly
                      0. 0. (/ blur 2.) (/ blur 2.) (cpaint/grey 225)))))))

(defn a-view []
  (let [font (Font. style/face-default (float 14))]
    (ui2/stack
     (ui2/fill-rect (huipaint/fill (cpaint/grey 245)))
     (ui2/column*
      (mapv (fn [[f n p]]
              (ui2/sized-with
               (fn [{:keys [scale]}]
                 (Point. 400. (+ (* (or scale 1) 4.) (Math/ceil (.getHeight (.getMetrics font))))))
               (ui2/stack
                ((ui2/direct-widget
                  {:draw
                   (fn [w {:keys [scale]} ^Rect rect ^Canvas cnv]
                     (let [idiff 16.
                           fil? (== p (dec n))
                           gl (+ 16. (* n idiff))
                           gw 2.
                           vw 10.
                           ;; ic (cpaint/grey 225)
                           ic (unchecked-int 0xFFc9dcf1);(cpaint/grey 220)
                           buc 0xdF1f5488
                           gpaint (doto (Paint.)
                                    (.setShader
                                     (Shader/makeLinearGradient
                                      gl 0. (+ gl gw) 0.
                                      (int-array [ic (cpaint/grey 245)]))))]
                       (when (< 0 n)
                         #_(.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                  (- 0 gw vw)) (:y rect)
                                               (+ (:x rect) (+ 16. (* n 16.))
                                                  (- vw))
                                               (:bottom rect))
                                    ;; (huipaint/fill ic)
                                    (doto (Paint.)
                                      (.setShader
                                       (Shader/makeLinearGradient
                                        (- (+ (:x rect) (+ 16. (* n 16.))) gw vw)
                                        0. (+ (:x rect) (+ 16. (* n 16.)) (- vw)) 0.
                                        (int-array [(unchecked-int 0) ic])))))
                         (.drawRect cnv (Rect. (:x rect) (:y rect)
                                               (+ (:x rect) (+ 16. (* n 16.))
                                                  (- vw))
                                               (:bottom rect))
                                    (huipaint/fill (- ic 0xda000000)))
                         #_(.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                  (- vw)) (:y rect)
                                               (+ (:x rect) (+ 16. (* n 16.)))
                                               (:bottom rect))
                                    (huipaint/fill ic))
                         (.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                  (- vw)) (:y rect)
                                               (+ (:x rect) (+ 16. (* n 16.)))
                                               (+ (:y rect) 6.))
                                    (huipaint/fill ic))
                         (.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                  (- vw)) (- (:bottom rect) 6.)
                                               (+ (:x rect) (+ 16. (* n 16.)))
                                               (:bottom rect))
                                    (huipaint/fill ic))
                         (when fil?
                           (.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                    (- 2.)) (:y rect)
                                                 (+  (:x rect) (+ 16. (* n 16.)))
                                                (+ (:y rect) 2.))
                                     (huipaint/fill buc)))
                         (.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                  (- 1)) (- (:bottom rect) 3.)
                                               (+ 1 (:x rect) (+ 16. (* n 16.)))
                                               (:bottom rect))
                                    (huipaint/fill buc)))
                       #_(.drawRect cnv (.withWidth (.offset rect (Point. gl 0.))
                                                  gw)
                                  gpaint)
                       (.drawRect cnv (Rect. (:x rect) (- (:bottom rect) 1)
                                             (+ (:x rect) gl) (:bottom rect))
                                  (huipaint/fill (cpaint/grey 200)))
                       (.drawRect cnv (Rect. (+ (:x rect) gl) (- (:bottom rect) 2)
                                             (+ (:x rect) gl idiff) (:bottom rect))
                                  (huipaint/fill #_(cpaint/grey 120)
                                                 buc))))})
                 {})
                (ui2/padded
                 [2. 2. 0 0]
                 (ui2/sized-with
                  (fn [_] (Point. 16. 16.))
                  (ui2/svg-from-data (maticons/svg-data "folder" "outlined" "24px"))))
                (ui2/padded
                 [20. 1. 6. (+ 3 (Math/ceil (.getDescent (.getMetrics font))))]
                 (ui2/textline (uifont/shape-line-default font (fs/file-name f)))))))
            (map vector
                 (fs/list-dir (io/file "."))
                 [0 0 0 1 1 2 3 4 4 1 2 2 2 1]
                 [0 0 0 0 1 1 2 3 4 4 1 2 2 2]))))))

(comment
  (windows/remount-all-windows)
  ;; ideas:
  ;; fade out underscore when folder not expanded
  #!
  )
