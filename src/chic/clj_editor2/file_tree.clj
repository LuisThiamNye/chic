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
     (ui2/fill-rect (huipaint/fill (unchecked-int 0xFFeff2f7)#_(cpaint/grey 245)))
     (ui2/column*
      (mapv (fn [[f n p]]
              (ui2/sized-with
               (fn [{:keys [scale]}]
                 (Point. 400. (+ (* (or scale 1) 4.) (Math/ceil (.getHeight (.getMetrics font))))))
               (ui2/stack
                ((ui2/direct-widget
                  {:draw
                   (fn [w {:keys [scale]} ^Rect rect ^Canvas cnv]
                     (let [idiff 17.
                           fil? (== p (dec n))
                           gl (+ 16. (* n idiff))
                           gw 2.
                           vw 10.
                           ;; ic (cpaint/grey 225)
                           ic (unchecked-int 0xFFc9dcf1) ;(cpaint/grey 220)
                           ;; buc 0xdF1f5488
                           ;; buc (unchecked-int 0xFF136ab5)
                           ;; buc (unchecked-int 0xFF8050b1)
                           ;; buc (unchecked-int 0xFF9062c5)
                           ;; buc (unchecked-int 0xFF9b6fcf)
                           buc (unchecked-int 0xFFb37bfa)
                           gpaint (doto (Paint.)
                                    (.setShader
                                     (Shader/makeLinearGradient
                                      gl 0. (+ gl gw) 0.
                                      (int-array [ic (cpaint/grey 245)]))))]
                       #_(dotimes [r (max 0 (dec n))]
                           (.drawRect cnv
                                      (Rect/makeXYWH
                                       (+ -2 (* (+ 2 r) idiff) (:x rect))
                                       (+ -3 (:bottom rect))
                                       2.
                                       4.)
                                      (huipaint/fill
                                       (- buc (* (- n r) 0x05000000)
                                          0xca000000))))
                       (dotimes [r n]
                         (let [left (+ -6 (* (+ 2 r) idiff) (:x rect))
                               right (+ 2 (* (+ 2 r) idiff) (:x rect))
                               ;; col (unchecked-int 0x104c9fe5)
                               col (unchecked-int 0xFFf3e4ff)
                               ;; col (unchecked-int 0xFFffffff)
                               col' (- col (* (- n r) 0x02000000))]
                           (.drawRect cnv
                                      (Rect.
                                       left (+ (:y rect))
                                       right (+ (:bottom rect)))
                                      (doto (Paint.)
                                        (.setShader
                                         (Shader/makeLinearGradient
                                          left 0. right 0.
                                          (int-array [(unchecked-int 0) col' (unchecked-int 0)])
                                          (float-array [0. 0.7 1.])))))
                           #_(.drawRect cnv
                                        (Rect.
                                         (+ -2 (* (+ 2 r) idiff) (:x rect))
                                         (+ (:y rect))
                                         (+ -1 (* (+ 2 r) idiff) (:x rect))
                                         (+ (:bottom rect)))
                                        (huipaint/fill col'))
                           #_(let [left (+ -1 (* (+ 2 r) idiff) (:x rect))
                                   right (+ 2 (* (+ 2 r) idiff) (:x rect))]
                               (.drawRect cnv
                                          (Rect.
                                           left (+ (:y rect))
                                           right (+ (:bottom rect)))
                                          (doto (Paint.)
                                            (.setShader
                                             (Shader/makeLinearGradient
                                              left 0. right 0.
                                              (int-array [col' (unchecked-int 0)]))))))))
                       #_(dotimes [r n]
                           (let [l (+ -6 (* (+ 2 r) idiff) (:x rect))
                                 right (+ -2 (* (+ 2 r) idiff) (:x rect))]
                             (.drawRect cnv
                                        (Rect.
                                         l (+ 1 (:y rect))
                                         right (+ -3 (:bottom rect)))
                                        (doto (Paint.)
                                          (.setShader
                                           (Shader/makeLinearGradient
                                            l 0. right 0.
                                            (int-array [(unchecked-int 0)
                                                        (- buc (* (- n r) 0x02000000)
                                                           0xe9000000)]))))))
                           (.drawRect cnv
                                      (Rect.
                                       (+ -2 (* (+ 2 r) idiff) (:x rect))
                                       (+ 1 (:y rect))
                                       (+ (* (+ 2 r) idiff) (:x rect))
                                       (+ -3 (:bottom rect)))
                                      (huipaint/fill
                                       (- buc (* (- n r) 0x02000000)
                                          0xe9000000))))
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
                                               (+ (:x rect) (+ 16. (* n idiff))
                                                  (- vw))
                                               (:bottom rect))
                                    (huipaint/fill (- ic 0xda000000)))
                         #_(.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
                                                    (- vw)) (:y rect)
                                                 (+ (:x rect) (+ 16. (* n 16.)))
                                                 (:bottom rect))
                                      (huipaint/fill ic))
                         #_#_(.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n 16.))
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
                           (.drawRect cnv (Rect. (+ 1. (:x rect) (+ 16. (* n idiff))
                                                    (- 2.))
                                                 (+ -1 (:y rect))
                                                 (+ 1. (:x rect) (+ 16. (* n idiff)))
                                                 (+ (:y rect) 2.))
                                      (huipaint/fill buc)))
                         (.drawRect cnv (Rect. (+ (:x rect) (+ 16. (* n idiff))
                                                  (- 1)) (- (:bottom rect) 3.)
                                               (+ 1 (:x rect) (+ 16. (* n idiff)))
                                               (+ -1 (:bottom rect)))
                                    (huipaint/fill buc)))
                       #_(.drawRect cnv (.withWidth (.offset rect (Point. gl 0.))
                                                    gw)
                                    gpaint)
                       #_(.drawRect cnv (Rect. (:x rect) (- (:bottom rect) 1)
                                               (+ (:x rect) gl) (:bottom rect))
                                    (huipaint/fill (cpaint/grey 200)))
                       (.drawRect cnv (Rect. (+ (:x rect) gl) (:bottom rect)
                                             (+ (:x rect) gl idiff) (+ 1 (:bottom rect)))
                                  (huipaint/fill (- buc 0x90000000)))
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
                 (ui2/textline (uifont/shape-line-default font (fs/file-name f))
                               (huipaint/fill 0xEc000000))))))
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
