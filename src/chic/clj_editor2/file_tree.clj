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
  (let [font (Font. style/face-default (float 14))
        ;; icols (mapv (fn [_] (cpaint/okhsv* (rand) 0.96 0.86)) (range 20))
        icols (mapv (fn [r] (cpaint/okhsv* (* r 0.2) 0.96 0.86)) (range 20))
        ]
    (ui2/stack
     (ui2/fill-rect (huipaint/fill (unchecked-int 0xFFeff2f7)#_(cpaint/grey 245)))
     (ui2/column*
      (mapv (fn [[f n p]]
              (let [iw 2.
                    idnt (* n iw)]
                (ui2/sized-with
                (fn [{:keys [scale]}]
                  (Point. 400. (+ (* (or scale 1) 4.) (* (or scale 1.) (Math/ceil (.getHeight (.getMetrics font)))))))
                (ui2/stack
                 ((ui2/direct-widget
                   {:draw
                    (fn [w {:keys [scale]} ^Rect rect ^Canvas cnv]
                      (let [idiff 17.
                            fil? (== p (dec n))
                            gl (+ 16. (* n idiff))
                            vw 10.
                            x0 (+ (:x rect) idnt)
                            ic (unchecked-int 0xFFc9dcf1)
                            buc (unchecked-int 0xFFb37bfa)]
                        (dotimes [r n]
                          (let [x (* (inc r) iw scale)
                                ;; col (if (even? r) buc (unchecked-int 0xFFd88013))
                                ;; col (cpaint/okhsv* (rand) 0.96 0.86)
                                col (nth icols r)
                                x-inner (- x (* scale 2.))
                                x-mid (- x (* scale 1.))]
                            (.drawRect cnv (Rect. x-inner  (:y rect) x-mid (:bottom rect))
                                       (huipaint/fill (- col 0x90000000)))
                            (.drawRect cnv (Rect. x-mid (:y rect) x (:bottom rect))
                                      (huipaint/fill col))
                            (when (== r (dec n))
                              (.drawRect cnv (Rect. x0 (:y rect) (+ x0 (* scale 1.)) (:bottom rect))
                                        (huipaint/fill col)))))
                        #_(when (< 0 n)
                          (.drawRect cnv (Rect. (:x rect) (:y rect)
                                                (+ (:x rect) (+ 16. (* n idiff))
                                                   (- vw))
                                                (:bottom rect))
                                     (huipaint/fill (- ic 0xda000000)))
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
                        (when fil?
                          (.drawRect cnv (Rect. x0 (- (:y rect) (* 2. scale)) (+ x0 8. (* n 8. scale))
                                                (:y rect))
                                     (huipaint/fill (nth icols (dec n)))))
                        #_#_
                        (.drawRect cnv (Rect. (+ (:x rect) gl) (:bottom rect)
                                              (+ (:x rect) gl idiff) (+ 1 (:bottom rect)))
                                   (huipaint/fill (- buc 0x90000000)))
                        (.drawRect cnv (Rect. (+ (:x rect) gl) (- (:bottom rect) 2)
                                              (+ (:x rect) gl idiff) (:bottom rect))
                                   (huipaint/fill #_(cpaint/grey 120)
                                                  buc))))})
                  {})
                 (ui2/padded
                  [(+ idnt 2.) 2. 0 0]
                  (ui2/sized-with
                   (fn [_] (Point. 16. 16.))
                   (ui2/svg-from-data (maticons/svg-data "folder" "outlined" "24px"))))
                 (ui2/padded
                  [(+ idnt 20.) 1. 6. (+ 3 (Math/ceil (.getDescent (.getMetrics font))))]
                  (ui2/textline (uifont/shape-line-default font (fs/file-name f))
                                (huipaint/fill 0xEc000000)))))))
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
