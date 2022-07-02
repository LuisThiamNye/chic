(ns chic.controls.textbox.cursor
  (:require
   [chic.style :as style]
   [clojure.math :as math]
   [potemkin :refer [doit]]
   [proteus :refer [let-mutable]]
   [chic.ui.font :as uifont]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas Path)
   (io.github.humbleui.types Rect Point RRect)))

(defn compute-selection-path [{:keys [line-height
                                      first-line-origin
                                      first-line-x
                                      line-end-xs
                                      border-radius]}
                              with-first-line?]
  (let [path (Path.)
        bd (* 2 border-radius)
        origin-x (:x first-line-origin)
        origin-y (:y first-line-origin)
        add-line-end (fn --ale [^Path path y x0 x1]
                       (when-not (== x0 x1)
                         (let [dx (min border-radius (abs (/ (- x0 x1) 2)))]
                           (if (< x1 x0)
                             (do (.arcTo path (Rect. (- x0 (* 2 dx)) (- y bd)
                                                    x0 y)
                                        0. 90. false)
                                (.lineTo path (+ x1 dx) y)
                                (.arcTo path (Rect. x1 y
                                                    (+ x1 (* 2 dx)) (+ y bd))
                                        270. -90. false))
                             (do (.arcTo path (Rect. x0 (- y bd)
                                                     (+ x0 (* 2 dx)) y)
                                        180. -90. false)
                                (.lineTo path (- x1 dx) y)
                                (.arcTo path (Rect. (- x1 (* 2 dx)) y
                                                    x1 (+ y bd))
                                        270. 90. false)))))
                       #_(.lineTo path x1 (+ y line-height (- border-radius))))
        complete (fn --complete [y x]
                   (let [l (min (* 2 border-radius) (- x origin-x))]
                     ;; bottom right
                     (.arcTo path (Rect. (- x l) (- y bd) x y)
                             0. 90. false)
                     (.lineTo path (+ origin-x border-radius) y)
                     (.arcTo path (Rect. origin-x (- y bd) (+ origin-x l) y)
                             90. 90. false)
                     #_(.lineTo path origin-x (+ line-height border-radius (:y first-line-origin)))))]
    (if with-first-line? ;; first line joined to rest
      (let [first-end-x (nth line-end-xs 0)
            l (min (- first-end-x first-line-x) bd)]
       (doto path
         ;; bottom left of start of first line
         (.moveTo first-line-x (- (+ origin-y line-height) border-radius))
         (.lineTo first-line-x (+ (:y first-line-origin) border-radius))
         (.arcTo (Rect. first-line-x (:y first-line-origin)
                        (+ first-line-x l)
                        (+ (:y first-line-origin) l))
                 180. 90. false)
         (.lineTo (- first-end-x border-radius) (:y first-line-origin))
         (.arcTo (Rect. (- first-end-x l) (:y first-line-origin)
                        first-end-x (+ (:y first-line-origin) l))
                 270. 90. false)
         (.lineTo (nth line-end-xs 0) (- (+ line-height origin-y) border-radius)))
       (doto (Path.)
         (add-line-end (+ origin-y line-height) first-line-x origin-x)
         (->> (.reverseAddPath path)))
       (add-line-end path (+ line-height origin-y) first-end-x (nth line-end-xs 1)))
      ;; ignore first line
      (let [end-x (nth line-end-xs 1)
            l (min (- end-x origin-x) bd)]
        (doto path
          (.moveTo origin-x (+ origin-y (/ l 2)))
         (.arcTo (Rect. origin-x (+ origin-y line-height)
                        (+ origin-x l)
                        (+ origin-y line-height l))
                 180. 90. false)
         (cond-> (< (- end-x origin-x) bd)
           (.lineTo (- end-x border-radius) (+ origin-y line-height)))
         (.arcTo (Rect. (- end-x l) (+ origin-y line-height)
                        end-x (+ origin-y line-height l))
                 270. 90. false)
         (.lineTo (nth line-end-xs 1) (- (+ origin-y (* 2 line-height)) border-radius)))))
    (let-mutable [y (+ (* 2 line-height) (:y first-line-origin))
                  prev-x (nth line-end-xs 1)]
      (doit [end-x (or (nnext line-end-xs) [])]
        (add-line-end path y prev-x end-x)
        (set! prev-x end-x)
        (set! y (+ y line-height)))
      (complete y prev-x))
    (.closePath path)))

[:paint]
(defn selction-draw-fn-for-layout [select-layout paint]
  (let [{:keys [line-height
                first-line-origin
                first-line-x
                line-end-xs]} select-layout
       br (Math/ceil (* 0.15 line-height))
       second-line-end-x (nth line-end-xs 1 nil)
       nlines (count line-end-xs)]
    (if (or (nil? second-line-end-x)
            (<= second-line-end-x first-line-x))
      (let [first-line-top (:y first-line-origin)
            x0 (:x first-line-origin)
            lone-fl-rrect (RRect/makeLTRB first-line-x first-line-top
                                          (nth line-end-xs 0) (+ first-line-top line-height)
                                          br)]
        (if (<= nlines 2)
          (let [lone-ll-rrect (when (== 2 nlines)
                                (RRect/makeLTRB x0 (+ first-line-top line-height)
                                                second-line-end-x (+ first-line-top (* 2 line-height))
                                                br))]
            ;; two lines, separated rects
            (fn --draw-selection [_ _ _ ^Canvas cnv]
              (.drawRRect cnv lone-fl-rrect paint)
              (when lone-ll-rrect
                (.drawRRect cnv lone-ll-rrect paint))))
          ;; >2 lines, with lone first-line rect
          (let [path (compute-selection-path (assoc select-layout :border-radius br) false)]
            (fn --draw-selection2 [_ _ _ ^Canvas cnv]
              (.drawRRect cnv lone-fl-rrect paint)
              (.drawPath cnv path paint)))))
      ;; with top line joined
      (let [path (compute-selection-path (assoc select-layout :border-radius br) true)]
        (fn --draw-selection3 [_ _ _ ^Canvas cnv]
          (.drawPath cnv path paint))))))

(def cursor-w
  (ui2/direct-widget
   {:get [:paint :line-top :line-height :cursor-x]
    :draw
    (fn [self {:keys [scale]} _ ^Canvas cnv]
      (let [line-top (ui2/access self :line-top)
            cursor-x (math/round (ui2/access self :cursor-x))]
        (.drawRect cnv (Rect. (- cursor-x scale) line-top
                              (+ cursor-x scale) (+ line-top (ui2/access self :line-height)))
                   (ui2/access self :paint))))}))

;; (defn cursor-widget)
