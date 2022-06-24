(ns chic.ui.ui2
  (:require
   [chic.style :as style]
   [io.github.humbleui.paint :as huipaint]
   [chic.util :as util]
   [chic.ui :as cui]
   [clojure.pprint :as pp]
   [proteus :refer [let-mutable]]
   [potemkin :refer [doit]]
   [clj-commons.primitive-math :as prim]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui])
  (:import
   (java.util ArrayList)
   (io.github.humbleui.skija Canvas Font Paint TextLine FontMetrics)
   (io.github.humbleui.skija.shaper ShapingOptions Shaper)
   (io.github.humbleui.types IPoint IRect Rect Point)
   (java.lang AutoCloseable)))

(defn -transmit-rect [widget ctx rect]
  (if-some [adapt-rect (get widget :adapt-rect)]
    (adapt-rect widget ctx rect)
    rect))

(defn draw [widget ctx rect ^Canvas canvas]
  ((:draw widget) widget ctx (-transmit-rect widget ctx rect) canvas))

(defn access [widget prop]
  ((get (:getters widget) prop) {}))

(defn direct-widget [config]
  (fn [opts]
    (assoc config :getters (:getters opts))))

"
layout:
- arrange
- align
constraints (max size) go down,
sizes go up,
parents set position
"

(defn layout [{} child]
  child)

(defn column [{} children]
  {:on-draw
   (fn [{} ctx rect ^Canvas canvas]
     (let-mutable [y 0]
       (doit [child children]
         (let [child-size (huip/-measure child ctx rect)]
           (huip/-draw child ctx (cui/offset-lt rect 0 y) canvas)
           (set! y (+ y (:height child-size)))))))})

(defn inf-column [{:keys [init prev] :as opts}]
  (let [build-next (:next opts)]
    (assert (fn? init)) (assert (fn? prev)) (assert (fn? build-next))
    (cui/generic
     {:init {:items []
             :children (ArrayList.)}
      :init-mut {:offset 0}
     ;; :on-measure
     ;; (fn [_ _ _])
      :on-draw
      (fn [{{:keys [children]} :d
            {:keys [offset]} :mut}
           ctx rect ^Canvas canvas]
        #_(build-next)

        (let-mutable [y offset]
          (doit [child children]
            (let [child-size (huip/-measure child ctx rect)]
              (huip/-draw child ctx (cui/offset-lt rect 0 y) canvas)
              (set! y (+ y (:height child-size)))))))})))

(def stack-w
  (direct-widget
   {:get [:children]
    :draw (fn [self ctx rect cnv]
            (doit [c (access self :children)]
              (draw c ctx rect cnv)))}))

(defn stack* [children]
  (stack-w {:getters {:children (fn [_] children)}}))

(defmacro stack [& children]
  `(stack* [~@children]))

(def fill-rect-w
  (direct-widget
   {:get [:paint]
    :draw (fn [self _ctx ^Rect rect ^Canvas cnv]
            (.drawRect cnv rect (access self :paint)))}))

(defn fill-rect [^Paint paint]
  (fill-rect-w {:getters {:paint (fn [_] paint)}}))

(def fill-rrect-w
  (direct-widget
   {:get [:paint]
    :draw (fn [self _ctx ^Rect rect ^Canvas cnv]
            (.drawRRect cnv (.withRadii rect (float(access self :radius)))
                        (access self :paint)))}))

(defn fill-rrect [br ^Paint paint]
  (fill-rrect-w {:getters {:paint (fn [_] paint)
                           :radius (fn [_] br)}}))

(defn subrect-sizer [width height])

#_(defn sized [width height child]
  (assoc child :adapt-rect ))

(defn adapt-rect [f child]
  (assoc child :adapt-rect
         (if-some [f0 (:adapt-rect child)]
           (fn [w ctx rect] (f0 w ctx (f ctx rect)))
           (fn [_ ctx rect] (f ctx rect)))))

(defn -rect-haligned [offset-coeff coeff parent rect]
  (let [x (+ (:x parent) (* coeff (:width parent)))
        offset (* offset-coeff (:width rect))
        l (- x offset)]
    (Rect. l (:y rect) (+ l (:width rect)) (:bottom rect))))

(defn v1-root [{:keys [on-mount]} ui]
  (let [*unmounted? (volatile! true)
        on-mount (or on-mount (fn [_ctx]))]
    (cui/generic
    {:draw
     (fn [_ ctx ^IRect rect ^Canvas cnv]
       (let [layer (.save cnv)]
         (.translate cnv (- (:x rect)) (- (:y rect)))
         (when @*unmounted?
           (on-mount ctx)
           (vreset! *unmounted? false))
         (draw ui ctx (.toRect rect) cnv)
         (.restoreToCount cnv layer)))
     :event (fn [_ evt]
              (if (and (:hui.event.mouse-button/is-pressed evt)
                       (cui/point-in-component? evt (:chic.ui/mouse-win-pos evt)))
                nil nil))
     :close (fn [_])})))

(def textline-w
  (direct-widget
   {:get [:paint :text-line]
    :draw (fn [self _ ^Rect rect ^Canvas cnv]
            (let [text-line ^TextLine (access self :text-line)]
              (.drawTextLine cnv text-line
                            #_x (:x rect)
                            #_y (:bottom rect)
                            (access self :paint))))}))
;; you already know the bounds of the TextLine

(defn ph-textline [text {:keys [background]}]
  (let [scale 2
        font (Font. style/face-code-default (float (* scale 12)))
        text-line (.shapeLine
                   cui/shaper text font
                   io.github.humbleui.skija.shaper.ShapingOptions/DEFAULT)]
    (adapt-rect
     (fn [_ ^Rect rect]
       (Rect/makeXYWH (:x rect) (:y rect)
                      (.getWidth text-line) (Math/ceil (.getCapHeight text-line))))
     (stack*
      (into []
            cat
            [background
             [(textline-w
               {:getters
                {:paint (constantly (huipaint/fill 0xff000000))
                 :text-line (constantly text-line)}})]])))))

(def padded-w
  {:get [:child]
   })

(defn margin [value child]
  (adapt-rect
   (fn [_ ^Rect rect] (.inflate rect (- value)))
   child))
