(ns chic.ui.ui2
  (:require
   [chic.ui.font :as uifont]
   [chic.debug]
   [chic.ui.interactor :as uii]
   [chic.style :as style]
   [io.github.humbleui.paint :as huipaint]
   [chic.util :as util]
   [chic.ui :as cui]
   [clojure.pprint :as pp]
   [chic.types :as types]
   [proteus :refer [let-mutable]]
   [potemkin :refer [doit]]
   [clj-commons.primitive-math :as prim]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui])
  (:import
   (io.lacuna.bifurcan LinearList)
   (io.github.humbleui.skija.svg SVGLengthContext SVGDOM SVGSVG SVGLengthType)
   (java.util ArrayList HashMap)
   (io.github.humbleui.skija Data Canvas Font Paint TextLine FontMetrics)
   (io.github.humbleui.skija.shaper ShapingOptions Shaper)
   (io.github.humbleui.types IPoint IRect Rect Point)
   (io.github.humbleui.jwm EventMouseMove EventTextInput EventKey)
   (java.lang AutoCloseable)))

"
Idea: replace on-mount and some of ctx with standardised builder function
that takes a mount ctx (contains jwm window, mutable mouse pos etc).
Or only use widgets that have a mutable slot for their parent, so that they
can get parent ctx in addition to window/global ctx.

Would reduce the need to have to save the ctx for use between draw/event fns

If winodw ctx, Makes sense to have the special mount function rather than a common
subscription-like dependency system because the window ctx should only change
if moved between windows (or when EventScreenChange so scale can be in win ctx).
"

(defmacro with-save [canvas & body]
  (assert (symbol? canvas))
  `(let [c# (.save ^Canvas ~canvas)]
     (try
       ~@body
       (finally (.restoreToCount ^Canvas ~canvas c#)))))

(defn -transmit-rect [widget ctx rect]
  (if-some [adapt-rect (get widget :adapt-rect)]
    (adapt-rect widget ctx rect)
    rect))

(defn draw [widget ctx rect ^Canvas canvas]
  ((:draw widget) widget ctx (-transmit-rect widget ctx rect) canvas))

(defn measure [widget ctx max-rect]
  (if-some [f (:measure widget)]
    (f widget (-transmit-rect widget ctx max-rect))
    max-rect))

(defn access [widget prop]
  #_(when (nil? (get (:getters widget) prop))
      (prn widget))
  ((get (:getters widget) prop) 0))

(defn get-getter [widget prop]
  (get (:getters widget) prop))

(defn direct-widget [config]
  (fn [opts]
    (assoc config :getters (:getters opts)
           :slots (:slots opts))))

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

(def column-w
  (direct-widget
   {:get [:children]
    :draw
    (fn [self ctx rect ^Canvas canvas]
      (let-mutable [y (:y rect)]
        (doit [child (access self :children)]
          (when (< y (:bottom rect))
            (let [child-rect (measure child ctx (Rect. (:x rect) y
                                                       (:right rect) (:bottom rect)))]
              (draw child ctx child-rect canvas)
              (set! y (+ y (:height child-rect))))))))}))

(defn column* [children]
  (column-w {:getters {:children (constantly children)}}))

(defmacro column [& children]
  `(column* [~@children]))

(def eqicolumn-w
  (direct-widget
   {:get [:children :height]
    :measure (fn [self ^Rect rect]
               (.withHeight rect (* (count (access self :children))
                                    (access self :height))))
    :draw
    (fn eqicolumn-draw [self ctx rect ^Canvas canvas]
      (let [^long height (access self :height)
            l (:x rect)
            t (:y rect)
            r (:right rect)
            b (:bottom rect)
            visible-rect (:visible-rect ctx rect)
            dt ^long (max 0 (- (:y visible-rect) t))
            idx0 (Math/floorDiv dt height)
            children (access self :children)
            end-idx (min (count children)
                          (Math/ceilDiv ^long (- (min b (:bottom visible-rect)) t) height))]
        (loop [i idx0]
          (when (< i end-idx)
            (let [child (nth children i)
                  y (+ t (* i height))]
              (draw child ctx (Rect. l y r (+ height y)) canvas)
              (recur (unchecked-inc-int i)))))))}))

(defn eqicolumn* [height children]
  (eqicolumn-w {:getters {:children (if (fn? children) children (constantly children))
                          :height (if (fn? height) height (constantly height))}}))

#_(def xyoffset-w
  (direct-widget
   {:get [:child :offset-x :offset-y]
    :draw
    (fn [self ctx rect ^Canvas cnv]
      )}))

(def inf-column-w
  (direct-widget
   {:get [:children :offset]
    :slots [:build-init :build-prev :build-next]
    :draw
    (fn --draw [self ctx rect cnv]
      (let [build-next (get (:slots self) :build-next)]
        (let-mutable [y (+ (:y rect) (access self :offset))
                      i 0]
          (loop []
            (when (and (< y (:bottom rect)) (< i 1000))
              (when-some [child (build-next ctx)]
                (let [child-rect (measure child ctx (Rect. (:x rect) y
                                                           (:right rect) Float/MAX_VALUE))]
                  (draw child ctx child-rect cnv)
                  (set! i (inc i))
                  (set! y (+ y (:height child-rect))))
                (recur))))
          #_(doit [child (access self :children)]
              (let [child-rect (measure child (Rect. (:x rect) y
                                                     (:right rect) Float/MAX_VALUE))]
                (draw child ctx child-rect cnv)
                (set! y (+ y (:height child-rect))))))))}))

(defn inf-column [{:keys [init prev] :as opts}]
  (let [build-next (:next opts)
        children (java.util.ArrayDeque.)
        *state (volatile! {:offset 0})]
    ;; (assert (fn? init)) (assert (fn? prev)) (assert (fn? build-next))
    (.add children (build-next {}))
    (inf-column-w
     {:getters {:children (constantly children)
                :offset (fn [_] (:offset @*state))}
      :slots {:build-init init :build-prev prev
              :build-next (fn [ctx]
                            (let [c (build-next ctx)]
                              (.add children c)
                              c))}})))

#_(def basic-frame-w
  (direct-widget
   {:get [:child]
    :draw
    (fn [self ctx rect cnv]
      (draw c ctx rect cnv))}))

(defn clip-rect [child]
  (let [draw-old (:draw child)]
    (assoc child :draw (fn [self ctx rect ^Canvas cnv]
                         (with-save cnv
                           (.clipRect cnv rect)
                           (draw-old self ctx rect cnv))))))

#_(defn inf-column [{:keys [init prev] :as opts}]
    (let [build-next (:next opts)]
      (assert (fn? init)) (assert (fn? prev)) (assert (fn? build-next))
      (cui/generic
       {:init {:items []
               :children (ArrayList.)}
        :init-mut {:offset 0}
     ;; :on-measure
     ;; (fn [_ _ _])
        :draw
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
    :draw (fn [self {:keys [scale]} ^Rect rect ^Canvas cnv]
            (.drawRRect cnv (.withRadii rect (* scale (float (access self :radius))))
                        (access self :paint)))}))

(defn fill-rrect [br ^Paint paint]
  (fill-rrect-w {:getters {:paint (fn [_] paint)
                           :radius (fn [_] br)}}))

;; (defn subrect-sizer [width height])

#_(defn sized [width height child]
    (assoc child :adapt-rect))

(defn adapt-rect [f child]
  (assoc child :adapt-rect
         (if-some [f0 (:adapt-rect child)]
           (fn [w ctx rect] (f0 w ctx (f ctx rect)))
           (fn [_ ctx rect] (f ctx rect)))))

(defn sized-with [f child]
  (adapt-rect
   (fn [ctx ^Rect rect]
     (let [size (f ctx)]
       (Rect/makeXYWH (:x rect) (:y rect)
                      (:width size) (:height size))))
   (assoc child :measure
          (fn [_ rect]
            (let [size (f 0)]
              (Rect/makeXYWH (:x rect) (:y rect)
                             (:width size) (:height size)))))))

(defn -rect-haligned [offset-coeff coeff parent rect]
  (let [x (+ (:x parent) (* coeff (:width parent)))
        offset (* offset-coeff (:width rect))
        l (- x offset)]
    (Rect. l (:y rect) (+ l (:width rect)) (:bottom rect))))

(defn v1-root [{:keys [on-mount]} ui]
  (let [*unmounted? (volatile! true)
        event-listeners (ArrayList.)
        on-mount (or on-mount (fn [_ctx]))
        mouse-pos (types/->XyIMunsync 0 0)
        *text-input-handler (volatile! nil)
        keys->locs (HashMap.)
        close-fns (ArrayList.)]
    (cui/generic
     {:draw
      (fn [_ ctx ^IRect rect ^Canvas cnv]
        (let [layer (.save cnv)]
          (.translate cnv (- (:x rect)) (- (:y rect)))
          (when @*unmounted?
            (on-mount ctx)
            (vreset! *unmounted? false))
          (draw ui (assoc ctx
                          :jwm-window (:window-obj (:chic/current-window ctx))
                          ::mouse-pos mouse-pos
                          ::register-close (fn [f] (.add close-fns f))
                          ::add-event-listener
                          (fn [typ f]
                            (case typ
                              :all (.add event-listeners f))))
                (.toRect rect) cnv)
          (.restoreToCount cnv layer)))
      :event (fn [_ evt]
               (let [jwm-evt (:raw-event evt)]
                 (doit [el event-listeners]
                   (el (merge
                        evt
                        {::mouse-pos mouse-pos
                         ::set-text-input-handler
                         (fn [f] (vreset! *text-input-handler f))})
                       jwm-evt))
                 (condp instance? jwm-evt
                   EventMouseMove
                   (let [jwm-evt ^EventMouseMove jwm-evt]
                     (types/reset-xyi mouse-pos (.getX jwm-evt) (.getY jwm-evt)))
                   ;; EventTextInput
                   ;; (when-some [f @*text-input-handler]
                   ;;   (f (.getText ^EventTextInput jwm-evt)))
                   ;; EventKey
                   ;; (let [ ])
                   nil)
                 nil))
      :close (fn [_] (doit [c close-fns] (c)))})))

(defn get-mouse-pos [ctx]
  (get ctx ::mouse-pos))

(def text-string-w
  (direct-widget
   {:get [:paint :string :font]
    :measure (fn [self ^Rect rect]
               (.withHeight rect (.getHeight (.getMetrics ^Font (access self :font)))))
    :draw (fn [self _ ^Rect rect ^Canvas cnv]
            (let [string ^String (access self :string)]
              (.drawString cnv string
                           #_x (:x rect)
                           #_y (:bottom rect)
                           (access self :font)
                           (access self :paint))))}))

(defn text-string
  ([string font] (text-string string font nil))
  ([string font paint]
   (text-string-w {:getters {:paint (constantly (or paint (huipaint/fill 0xff000000)))
                             :string (constantly string)
                             :font (constantly font)}})))

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

(defn textline
  ([text-line] (textline text-line nil))
  ([text-line paint]
   (textline-w {:getters {:paint (constantly (or paint (huipaint/fill 0xff000000)))
                          :text-line (constantly text-line)}})))

(defn ph-textline [text {:keys [background]}]
  (let [scale 2
        font (Font. style/face-code-default (float (* scale 12)))
        text-line (uifont/shape-line-default font text)]
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

#_(def padded-w
    {:get [:child]})

(defn -inset-value->ltrb [value]
  (if (vector? value)
    (if (== 4 (count value))
      value
      (-> value (conj (nth value 0))
          (conj (nth value 1))))
    [value value value value]))

(defn padded [value child]
  (let [[l t r b] (-inset-value->ltrb value)]
    (adapt-rect
     (fn [{:keys [scale]} ^Rect rect]
       (Rect. (+ (* scale l) (:x rect)) (+ (* scale t) (:y rect))
              (- (:right rect) (* scale r)) (- (:bottom rect) (* scale b))))
     child)))

(defn padded-unscaled [value child]
  (let [[l t r b] (-inset-value->ltrb value)]
    (adapt-rect
     (fn [{:keys [scale]} ^Rect rect]
       (Rect. (+ l (:x rect)) (+ t (:y rect))
              (- (:right rect) r) (- (:bottom rect) b)))
     child)))

(defn before-draw [f child]
  (let [old-draw (:draw child)]
    (assoc child :draw (fn [w ctx rect cnv]
                         (f ctx rect)
                         (old-draw w ctx rect cnv)))))

(defn watch-rect [f child]
  (let [*old-rect (proteus.Containers$O. nil)]
    (before-draw
     (fn [ctx ^Rect rect]
       (when-not (.equals rect (.-x *old-rect))
         (.set *old-rect rect)
         (f ctx rect)))
     child)))

(defn on-mount
  ([fopen fclose child]
   (let [old-draw (:draw child)
         *unmounted? (volatile! true)]
     (assoc child :draw (fn [self ctx rect cnv]
                          (when @*unmounted?
                            (fopen ctx)
                            (when-some [r (and fclose (::register-close ctx))]
                              (r (fn [] (fclose child))))
                            (vreset! *unmounted? false))
                          (old-draw self ctx rect cnv)))))
  ([f child]
   (on-mount f nil child)))

(defn attach-interactor [{:keys [cursor-style
                                 on-mouse-down
                                 on-mouse-up
                                 on-scroll]
                          :as intr-opts}
                         child]
  (let [*intr-id (volatile! nil)]
    (on-mount
     (fn [ctx]
       (when-some [mgr (::interactor-manager ctx)]
         (vreset! *intr-id
                  (uii/mgr-create-intr mgr (assoc intr-opts
                                                  :rect (Rect. 0 0 0 0))))))
     (watch-rect
      (fn [ctx rect]
        (uii/mgr-intr-set-rect (::interactor-manager ctx) @*intr-id rect))
      child))))

(defn updating-ctx [f child]
  (let [old-draw (:draw child)]
    (assoc child :draw (fn [self ctx rect cnv]
                         (old-draw self (f ctx rect) rect cnv)))))

(defn attach-interactor-manager [_opts child]
  (let [*mgr (volatile! nil)]
    (on-mount
     (fn [ctx]
       ((::add-event-listener ctx)
        :all
        (fn [ctx evt]
          (uii/mgr-handle-jwm-event @*mgr ctx evt)))
       (vreset! *mgr (uii/new-mgr {:jwm-window (:jwm-window ctx)})))
     (updating-ctx (fn [ctx _] (assoc ctx ::interactor-manager @*mgr))
                   child))))

(defn on-event [f child]
  (on-mount
   (fn [ctx]
     ((::add-event-listener ctx)
      :all f)) ;; [ctx evt]
   child))

(def svg-w
  (direct-widget
   {:get [:svgdom]
    :draw (fn [self ctx rect ^Canvas canvas]
            (let [dom ^SVGDOM (access self :svgdom)
                  root ^SVGSVG (.getRoot dom)
                  lc (SVGLengthContext. (Point. (:width rect) (:height rect)))
                  width (.resolve lc (.getWidth root) SVGLengthType/HORIZONTAL)
                  height (.resolve lc (.getHeight root) SVGLengthType/VERTICAL)
                  xscale (/ (:width rect) width)
                  yscale (/ (:height rect) height)]
              (with-save canvas
                (.translate canvas (:x rect) (:y rect))
                (.setContainerSize dom (Point. (:x rect) (:y rect)))
                (.scale canvas xscale yscale)
                (.render dom canvas))))}))

(defn svg-from-data [^Data data]
  (svg-w {:getters {:svgdom (constantly (SVGDOM. data))}}))
