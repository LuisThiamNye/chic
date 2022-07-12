(ns chic.clj-editor2.file-tree
  (:require
   [babashka.fs :as fs]
   [clojure.walk :as walk]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.ui.ui2 :as ui2]
   [chic.ui.ui3 :as ui3]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.window :as huiwin]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.windows :as windows])
  (:import
   (java.nio.file Files)
   (io.github.humbleui.jwm Window)
   (io.github.humbleui.skija.svg SVGDOM SVGSVG SVGLength)
   (io.github.humbleui.skija Paint Shader Canvas ClipMode Font ImageFilter Path Image Surface)
   (io.github.humbleui.types Rect Point)))

;; (fs/list-dir (io/file "."))

#_(with-open [ds (Files/newDirectoryStream (fs/path "."))]
    (mapv str ds))

(comment
  (def --code1
    '(ui3/fnlet-widget
      (fn ui-icon-and-label
        [centre-y x height
         textblob text-paint cap-height wh->icon-image]
        (let [baseline-y (+ centre-y (/ cap-height 2))
              text-x (+ x (* util/phi height))
              icon-image (let [length (min height (* 1.2 cap-height))]
                           ^Image (wh->icon-image length length))
              icon-image-rect
              (let [iheight (.getHeight icon-image)
                    iwidth (.getWidth icon-image)]
                (Rect/makeXYWH (+ x (/ (- height iwidth) 2)) (- centre-y (/ iheight 2)) iwidth iheight))]
          {:draw
           (fn [cnv]
             (.drawImageRect cnv icon-image icon-image-rect)
             (.drawTextBlob cnv textblob text-x baseline-y text-paint))}))))
  (def ui-icon-and-label (eval --code1))
  (binding [*print-meta* true]
    (chic.debug/println-main
     (zprint.core/zprint-str
      (macroexpand-1
       --code1)
      120)))

  (def --c1 (resolve 'UiIconAndLabel))
  (= --c1 (resolve 'UiIconAndLabel))

  (map #(.getType ^java.lang.reflect.Field %)
       (.getDeclaredFields ^Class (resolve 'UiIconAndLabel)
                           #_(Class/forName (str (munge (str *ns*)) "." 'UiIconAndLabel))))

  (deftype UiCmptIconAndLabel [^float baseline-y ^float text-x ^Image icon-image icon-image-rect]
    (draw [_ cnv change-mask
           centre-y x height textblob text-paint
           cap-height wh->icon-image]
      (when (< 0 change-mask)
        (let [field-change-mask
              (-> 0
                  (cond-> (< 0 (bit-and change-mask 2r100001))
                    (bit-or (do (set! baseline-y (+ centre-y (/ cap-height 2)))
                                2r1))))]))
      (.drawImageRect cnv icon-image icon-image-rect)
      (.drawTextBlob cnv textblob text-x baseline-y text-paint))))

#_(let [s 10]
    (/ (.getCapHeight (.getMetrics (Font. style/face-code-default (float s))))
       s))

(defn swap-let-binding-input-syms [smap bindings]
  (:bindings
   (reduce
    (fn [{:keys [remaps bindings] :as acc} [sym vexpr]]
      (let [vexpr (walk/postwalk-replace remaps vexpr)]
        (if (contains? remaps sym)
          {:bindings (conj bindings [sym vexpr])
           :remaps (dissoc remaps sym)}
          (update acc :bindings conj [sym vexpr]))))
    {:remaps smap :bindings []}
    bindings)))

(defmacro fnlet-snippet [fexpr]
  (let [input-syms (some #(when (vector? %) %) fexpr)
        letexpr (last fexpr)
        ret-syms (last letexpr)
        ret-syms-set (set ret-syms)
        bindings (:bindings
                  (reduce
                   (fn [{:keys [remaps bindings] :as acc} [sym vexpr :as pair]]
                     (let [vexpr (walk/postwalk-replace remaps vexpr)]
                       (if (contains? ret-syms-set sym)
                         (update acc :bindings conj [sym vexpr])
                         (let [sym2 (gensym (str sym "_PRIV"))]
                           {:bindings (conj bindings [sym2 vexpr])
                            :remaps (assoc remaps sym sym2)}))))
                   {:remaps {} :bindings []}
                   (eduction (partition-all 2) (second letexpr))))]
    `(quote {:input-syms ~input-syms
             :bindings ~bindings
             :ret-syms ~ret-syms-set})))

(def calc-list-positions-fnsnip
  (fnlet-snippet
   (fn [rect visible-rect
        nchildren offset-y offset-x item-height]
     (let [content-y (+ (:y rect) offset-y)
           first-visible-idx (max 0 (Math/floorDiv (unchecked-int (- (:y visible-rect) content-y)) item-height))
           last-visible-idxe (min nchildren
                                  (Math/ceilDiv (unchecked-int (- (min (:bottom rect) (:bottom visible-rect))
                                                                   content-y))
                                                item-height))
           content-x (+ (:x rect) offset-x)]
       [first-visible-idx last-visible-idxe content-x content-y]))))

(defmacro inline-fnsnip-multiretmap [varsym direct-syms & [smap]]
  (let [{:keys [bindings input-syms ret-syms]} @(resolve varsym)
        smap (or smap {})
        input-smap (update-keys smap symbol)]
    (assert (= (set direct-syms)
               (set (remove #(contains? input-smap %) input-syms))))
    `(let ~(into [] cat
                 (swap-let-binding-input-syms input-smap bindings))
      ~(zipmap (map keyword ret-syms) ret-syms))))

(comment
  (let [offset-y 0 offset-x 0 item-height (int 10) visible-rect (Rect. 0 0 30 30) rect (Rect. 0 0 50 50)]
    (inline-fnsnip-multiretmap
     calc-list-positions-fnsnip
     [rect visible-rect offset-y offset-x item-height] {:nchildren 1}))
  ;; detect Closeables

  (deftype ___ [children]
    (draw [_ ...]
      (doit [c children]
        (draw c ...)))
    (recalculate [_ change-mask ...]
      (bit-test change-mask 0))
    (close [_] (doit [c children]
                 (.close c))))

  (def ui-linear-treeview
    (fnlet-widget
     (fn [rect visible-rect scale
          children-data offset-y offset-x item-height]
       (let [{:keys [first-visible-idx last-visible-idxe content-x content-y]}
             (let [nchildren (count children-data)]
               (inline-fnsnip-multiretmap
                calc-list-positions-fnsnip
                [rect visible-rect offset-y offset-x item-height nchildren]))
             font (Font. style/face-default ^float (* scale 14))
             text-paint (huipaint/fill 0xEc000000)
             ui-children (map-children-indexed
                          {:keyfn :filename}
                          (fn [i {:keys [filename]}]
                            (ui-icon-and-label
                             {:centre-y (+ content-y (* (+ 0.5 i) item-height))
                              :x content-x
                              :height item-height
                              :textblob (.getTextBlob (uifont/shape-line-default font filename))
                              :text-paint text-paint
                              :cap-height (.getCapHeight (.getMetrics font))
                              :wh->icon-image
                              (fn [^int w ^int h]
                                (with-open [surface (Surface/makeRasterN32Premul w h)]
                                  (let [dom (with-open [data (maticons/svg-data (if (== 0 (math/round (rand)))
                                                                                  "description"
                                                                                  "folder") "outlined" "24px")]
                                              (SVGDOM. data))
                                        root (.getRoot dom)]
                                    (.setWidth root (SVGLength. w))
                                    (.setHeight root (SVGLength. h))
                                    (.render dom (.getCanvas surface))
                                    (.makeImageSnapshot surface))))}))
                          children-data)]
         {:draw
          (fn [cnv]
            (draw-children ui-children cnv))}))))

  (mapv
   #(zipmap [:level :filename] %)
   [[0 ".lsp"]
    [0 "resources/clj"]
    [0 "src"]
    [1 "chic"]
    [2 "clj"]
    [2 "clj_editor"]
    [3 "core.clj"]
    [2 "controls"]
    [3 "textbox"]
    [4 "cursor.clj"]
    [4 "keybindings.clj"]
    [1 "oclj"]
    [0 "dir-locals.el"]
    [0 ".gitignore"]]))

(defn a-view []
  (let [font (Font. style/face-default (float 14))
        ;; icols (mapv (fn [_] (cpaint/okhsv* (rand) 0.96 0.86)) (range 20))
        icols (mapv (fn [r] (cpaint/okhsv* (* r 0.2) 0.96 0.86)) (range 20))
        icon-label-gap 3.
        item-pad-left 5.
        icon-width 16.]
    (ui2/on-mount
     (fn [{:keys [scale]}]
       (.setSize font (* 14. scale)))
     (ui2/stack
      (ui2/fill-rect (huipaint/fill (unchecked-int 0xFFeff2f7)))
      ((ui2/direct-widget
        {:draw (fn [_ _ rect cnv])}))
      #_(ui2/column*
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
                        (let [fil? (== p (dec n))
                              x0 (+ (:x rect) idnt)]
                          (dotimes [r n]
                            (let [x (* (inc r) iw scale)
                                  ;; col (if (even? r) buc (unchecked-int 0xFFd88013))
                                  ;; col (cpaint/okhsv* (rand) 0.96 0.86)
                                  col (nth icols r)
                                  x-inner (- x (* scale 2.))
                                  x-mid (- x (* scale 1.))]
                              (.drawRect cnv (Rect. x-inner (:y rect) x-mid (:bottom rect))
                                         (huipaint/fill (- col 0x20000000)))
                              (.drawRect cnv (Rect. x-mid (:y rect) x (:bottom rect))
                                         (huipaint/fill col))
                              (when (== r (dec n))
                                (.drawRect cnv (Rect. x0 (:y rect)
                                                      (+ x0 (* scale 2.)) (:bottom rect))
                                           (huipaint/fill col))
                                (.drawRect cnv (Rect. (+ x0 (* scale 2.)) (:y rect)
                                                      (+ x0 (* scale 3.)) (:bottom rect))
                                           (huipaint/fill (- col 0x90000000))))))
                          (when fil?
                            (let [stick-y (- (:y rect) (* (:height rect) 0.3))
                                  uh 4.]
                              ;; underline
                              (.drawPath cnv
                                         (doto (Path.)
                                           (.moveTo (+ x0 (* scale 10.))
                                                    (- (:y rect) (* uh scale)))
                                           (.lineTo (+ uh x0 8. (* n 8. scale))
                                                    (- (:y rect) (* uh scale)))
                                           (.lineTo (+ x0 8. (* n 8. scale))
                                                    (:y rect))
                                           (.lineTo (+ x0 (* scale 6.))
                                                    (:y rect))
                                           (.lineTo (Point. (+ x0 (* scale 2.)) (:y rect)))
                                           (.lineTo (Point. (+ x0 (* scale 3.)) (- (:y rect) (* (:height rect) 0.5)))))
                                         (huipaint/fill (- (nth icols (dec n))
                                                           0xC0000000)))
                              ;; tip behind
                              (.drawRect cnv (Rect. (- x0 (* scale 2.))
                                                    (+ 1. stick-y)
                                                    (- x0 (* 1. scale))
                                                    (:y rect))
                                         (huipaint/fill (- (nth icols (dec n)) 0x20000000)))
                              ;; tip rect
                              (.drawRect cnv (Rect. (- x0 (* scale 1.))
                                                    stick-y
                                                    (+ x0 (* 3. scale))
                                                    (:y rect))
                                         (huipaint/fill (nth icols (dec n))))
                              ;; tip
                              (.drawPath
                               cnv (doto (Path.)
                                     (.moveTo (Point. (+ x0 (* scale 3.)) (- (:y rect) (* (:height rect) 0.5))))
                                     (.lineTo (Point. (- x0 (* scale 1.)) (- (:y rect) (* (:height rect) 0.3))))
                                     (.lineTo (Point. (+ x0 (* scale 2.)) (:y rect)))
                                     (.lineTo (Point. (+ x0 (* scale 3.)) (- (:y rect) (* (:height rect) 0.2)))))
                               (huipaint/fill (nth icols (dec n))))))))})
                    {})))))
             (map vector
                  (fs/list-dir (io/file "."))
                  [0 0 0 1 1 2 3 4 4 1 2 2 2 1]
                  [0 0 0 0 1 1 2 3 4 4 1 2 2 2])))))))

(comment
  (windows/remount-all-windows)

;; ideas:
  ;; fade out underscore when folder not expanded

  ;; watching file tree:
  "
on modify:
  maybe update single file name
on delete/create:
  if fully below top: shift entries below
  if above that: shift entries above
on overflow: rebuild

lazy loading & watching:
  always keep in memory: visible items, all parents, (some) offscreen siblings of top & bottom.
  when scrolling, check if new items are expanded
    if so, get first/last child and recur
  chunking.
"
  #!
  )
(comment
  (deftype ____ [input-provider ;; field or argument?
                 ^int change-mask ;; ^
                 ^float baseline-y ^float text-x ^Image icon-image icon-image-rect]
    (draw [_ ^Canvas cnv]
      '...)
    (notify-changed [_ ks] ;; only if input-provider is field
      ;; notify when inputs changed
      ;; walk dep graph and mark changed properties
      '...)
    (recalculate [_ input-provider change-mask])
    (recalculate-all [_ input-provider])
    ;; or
    (recalc2 [_ centre-y x height textblob ...etc]))
  ;; or just use identical? diffing

  ;; separate objects for active values?
  ;; method to pass prev&new values and return a change mask - useful perhaps for repeated components
  ;; pass each input as a separate argument to avoid additional method calls.
  ;; One param could be reserved for the root context (eg mouse pos, jwm window)

  ;; There are inputs and there are dependent values (latter are functions of input and do not change directly).
  ;; But you may want mutable properties that can be directly set.

  ;; Could have active values that have a reference to a list of their dependents.
  ;; When active value is directly modified, all dependents are walked and their change field incremented
  ;; The directly modified values are stored as entry points for recalculation.
  ;; The next frame, all with change>=1 are recalculated, but ordered by the change field (1s then 2s etc)
  ;; Maybe this could help the case where two independent active values share the same dependents
  ;; But is complicated
  ;; Could also have an option to directly recalculate on modifying a value.
  ;; Probably should start with a more functional tree model.

  (deftype ____ [^float baseline-y ^float text-x ^Image icon-image icon-image-rect]
    (draw [_ inputs... ^Canvas cnv]
      '...)
    (recalculate [_ inputs... change-mask]))
  #!
  )
