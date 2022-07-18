(ns chic.clj-editor2.file-tree
  (:require
   [babashka.fs :as fs]
   [chic.debug :as debug]
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
   [chic.util :as util :refer [doit-zip]]
   [potemkin :refer [doary doit]]
   [chic.windows :as windows])
  (:import
   (java.nio.file Files)
   (io.lacuna.bifurcan List)
   (io.github.humbleui.jwm Window)
   (io.github.humbleui.skija.svg SVGDOM SVGSVG SVGLength)
   (io.github.humbleui.skija PaintStrokeCap Paint Shader Canvas ClipMode Font ImageFilter Path Image Surface)
   (io.github.humbleui.types Rect Point)))

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

(defmacro inline-fnsnip-multiretmap [varsym direct-syms & [smap]]
  (let [{:keys [bindings input-syms ret-syms]} @(resolve varsym)
        smap (or smap {})
        input-smap (update-keys smap symbol)]
    (assert (= (set direct-syms)
               (set (remove #(contains? input-smap %) input-syms))))
    `(let ~(into [] cat
                 (swap-let-binding-input-syms input-smap bindings))
       ~(zipmap (map keyword ret-syms) ret-syms))))

(do
  (def --code1
    (util/quoted
     (ui3/fnlet-widget
      (fn ui-icon-and-label
        [centre-y x height
         textblob text-paint cap-height wh->icon-image]
        (let [baseline-y (+ centre-y (/ cap-height 2))
              icon-length (min height (* 1.2 cap-height))
              icon-image ^Image (wh->icon-image icon-length icon-length)
              icon-x (+ x (/ (- height icon-length) 2))
              text-x (+ icon-x (* util/phi icon-length))
              icon-image-rect
              (let [iheight (.getHeight icon-image)
                    iwidth (.getWidth icon-image)]
                (Rect/makeXYWH icon-x (- centre-y (/ iheight 2)) iwidth iheight))]
          {:draw
           (fn [cnv]
             (.drawImageRect cnv icon-image icon-image-rect)
             (.drawTextBlob cnv textblob text-x baseline-y text-paint))})))))
  (def ui-icon-and-label (eval --code1)))

(comment
  (binding [*print-meta* true])
  #!
  )

(def ui-icon-and-label-apt
  (ui3/fnlet-widget
   (fn ui-icon-and-label-apt
     [idx filename cap-height item-height content-y x text-paint font]
     (let [textblob (.getTextBlob (uifont/shape-line-default font filename))
           cmpt (ui3/new-cmpt ui-icon-and-label)]
       {:draw
        (fn [cnv]
          ;; (debug/println-main content-y idx)
          (ui3/draw-cmpt
           cmpt cnv
           {:centre-y (+ content-y (* (+ 0.5 idx) item-height))
            :x x
            :height item-height
            :textblob textblob
            :text-paint text-paint
            :cap-height cap-height
            :wh->icon-image
            (fn [^long w ^long h]
              (let [w (unchecked-int w)
                    h (unchecked-int h)]
                (with-open [surface (Surface/makeRasterN32Premul w h)]
                  (let [dom (with-open [data (maticons/svg-data "description" "outlined" "24px")]
                              (SVGDOM. data))
                        root (.getRoot ^SVGDOM dom)]
                    (.setWidth root (SVGLength. w))
                    (.setHeight root (SVGLength. h))
                    (.render ^SVGDOM dom (.getCanvas surface))
                    (.makeImageSnapshot surface)))))}))}))))

(def calc-list-positions-fnsnip
  (fnlet-snippet
   (fn [rect visible-rect
        nchildren offset-y offset-x ^int item-height]
     (let [content-y (+ (:y rect) offset-y)
           first-visible-idx (max 0 (Math/floorDiv (unchecked-int (- (:y visible-rect) content-y)) item-height))
           last-visible-idxe (min nchildren
                                  (Math/ceilDiv (unchecked-int (- (min (:bottom rect) (:bottom visible-rect))
                                                                  content-y))
                                                item-height))
           content-x (+ (:x rect) offset-x)]
       [first-visible-idx last-visible-idxe content-x content-y]))))

(def icols (mapv (fn [r] (cpaint/okhsv* (* r 0.2) 0.96 0.86)) (range 20)))

(do
  (def --indent-vline-code
    (util/quoted
     (ui3/fnlet-widget
      (fn ui-indent-vline
        [^float scale ^float content-x level ^float first-child-top ^float parent-top
         ^float last-child-bottom]
        (let [indent-x (+ content-x (* level 2. scale))
              icon-centre-y (float (/ (+ first-child-top parent-top) 2))
              icon-centre-x (float (+ indent-x (/ (- first-child-top parent-top) 2)))
              thickness (* 3. scale)
              botrad (+ thickness (* scale 2.))
              vline-rect (Rect. indent-x first-child-top
                                (+ indent-x thickness) (- last-child-bottom botrad))
              vline-bleed-rect (Rect. (+ indent-x thickness) first-child-top
                                      (+ indent-x thickness (* scale 1.)) last-child-bottom)
              vline-shine-rect (Rect. indent-x first-child-top
                                      (+ indent-x (* scale 1.)) (- last-child-bottom botrad))
              paint (doto (huipaint/fill (nth icols level))
                      (.setStrokeWidth thickness)
                      (.setStrokeCap PaintStrokeCap/ROUND))
              shine-paint (huipaint/fill 0x10ffffff)
              bleed-paint (huipaint/fill (- (nth icols level) 0x90000000))
              bottom-path (doto (Path.)
                            (.moveTo indent-x (- last-child-bottom botrad))
                            (.arcTo (Rect. indent-x (- last-child-bottom (* 2 botrad))
                                           (+ indent-x (* 2 botrad)) last-child-bottom)
                                    180. -90. false)
                            (.lineTo (+ indent-x thickness botrad) last-child-bottom)
                            (.lineTo (+ indent-x thickness) (- last-child-bottom botrad)))]
          {:draw
           (fn [cnv]
             (.drawLine cnv icon-centre-x icon-centre-y
                        (+ indent-x (* 0.5 thickness)) first-child-top
                        paint)
             (.drawCircle cnv icon-centre-x icon-centre-y (* 1.5 scale thickness) paint)
             (.drawRect cnv vline-bleed-rect bleed-paint)
             (.drawRect cnv vline-rect paint)
             (.drawRect cnv vline-shine-rect shine-paint)
             (.drawPath cnv bottom-path paint))})))))
  (def ui-indent-vline (eval --indent-vline-code)))

(comment
  (binding [*print-meta* true *unchecked-math* true]
    (debug/puget-prn (macroexpand --indent-vline-code)))
  #!
  )

(defn children-data->group-ranges [children-data]
  (let [it (util/to-iter children-data)]
    (loop [i 0 prev-level nil
           parents []
           groups []]
      (let [child (util/iter-next it)]
        (if-some [level (or (:level child)
                            (when (== i (count children-data)) 0))]
          (let [level-change (- level (or prev-level level))]
            (recur (unchecked-inc i)
                   level
                   (if (== 1 level-change)
                     (conj parents (dec i))
                     (subvec parents 0 (+ (count parents) (min 0 level-change))))
                   (if (< level-change 0)
                     (into groups (map (fn [p] [p i]))
                           (subvec parents
                                   (+ (count parents) level-change)
                                   (count parents)))
                     groups)))
          (vec (sort-by first groups)))))))

(do
  (def --tree-code
    (util/quoted
     (ui3/fnlet-widget
      (fn ui-linear-treeview [^Rect rect ^Rect visible-rect ^float scale
                              children-data ^int offset-y ^int offset-x ^int item-height]
        (let [{:keys [first-visible-idx last-visible-idxe content-x content-y]}
              (let [nchildren (count children-data)]
                (inline-fnsnip-multiretmap
                 calc-list-positions-fnsnip
                 [rect visible-rect offset-y offset-x item-height nchildren]))
              font (Font. style/face-default
                          (unchecked-float
                           (unchecked-multiply
                            scale (unchecked-float
                                   (uifont/caph->size style/face-default 10.)))))
              cap-height (.getCapHeight (.getMetrics font))
              text-paint (huipaint/fill 0xEc000000)
              ui-children (mapv (fn [x] (ui3/new-cmpt ui-icon-and-label-apt))
                                children-data)
              group-ranges (children-data->group-ranges children-data)
              vline-cmpts (mapv (fn [_] (ui3/new-cmpt ui-indent-vline))
                                group-ranges)]
          {:draw
           (fn [cnv]
             (doit-zip [{:keys [level filename]} (eduction children-data)
                        i (util/int-range-it)]
               (ui3/draw-cmpt ^{:cmpt ui-icon-and-label-apt}
                (nth ui-children i) cnv
                              {:idx i
                               :filename filename
                               :cap-height cap-height
                               :item-height item-height
                               :content-y content-y
                               :x (+ (* scale 2 level) content-x)
                               :text-paint text-paint
                               :font font}))
             (doit-zip [[start ende] ^Iterable group-ranges
                        cmpt ^Iterable vline-cmpts]
               (ui3/draw-cmpt ^{:cmpt ui-indent-vline} cmpt cnv
                              {:scale scale
                               :content-x content-x
                               :level (:level (nth children-data start))
                               :first-child-top (+ content-y (* item-height (inc start)))
                               :parent-top (+ content-y (* item-height start))
                               :last-child-bottom (+ content-y (* item-height ende))})))})))))
  (def ui-linear-treeview (eval --tree-code)))

(comment
  (binding [*print-meta* true]
    (debug/puget-prn
     (macroexpand --tree-code)))
  (def --data (ui3/fnlet-widget-parse* (second --tree-code)))
  (-> --data
      :bindings butlast last)


  (def --data (debug/with-capture-data
                (macroexpand --tree-code)))
  (-> --data :draw-cmpt last :args
      (->> (filter (comp #{'parent-top} :sym)))
      first)

  #!
  )

(def sample-children-data (mapv
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

(do
  (def --a-view-code
    (util/quoted
     (let [font (Font. style/face-default (float 14))
           children-data sample-children-data
           ui-list (ui3/new-cmpt ui-linear-treeview)]
       (ui2/on-mount
        (fn [{:keys [scale]}]
          (.setSize font (* 14. scale)))
        (ui2/stack
         (ui2/fill-rect (huipaint/fill (unchecked-int 0xFFeff2f7)))
         (let [input-memory (ui3/cmpt-ext-input-memory ui-list)]
           ((ui2/direct-widget
             {:draw (fn [_ {:keys [scale]} rect ^Canvas cnv]
                      (ui3/draw-cmpt-ext-memo
                       ui-list cnv input-memory
                       {:rect rect
                        :visible-rect rect
                        :scale scale
                        :children-data children-data
                        :offset-y 0.
                        :offset-x 0.
                        :item-height (Integer. (int (* scale 20)))}))}) {})))))))
  (defn a-view []
    (util/compile --a-view-code)))

(comment
  (windows/remount-all-windows)
  (debug/puget-prn
   (clojure.tools.analyzer.jvm/macroexpand-all --a-view-code))

  ;; detect Closeables
  ;; identify temp bindings that are not used in the draw function - make them local, not fields

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

  ;; idea: initConstants method for component so consumer can pass inputs that do not change
  ;; so the component can initialise fields that depend only on the constant inputs
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

(defn bcoll-search-insertion-idx
  ^long [^java.util.Comparator cmptor ^io.lacuna.bifurcan.ICollection coll k]
  (loop [min-idx 0
         max-idx (.size coll)]
    (if (== min-idx max-idx)
      max-idx
      (let [idx (unchecked-add
                 min-idx (unchecked-int
                          (Math/floor (unchecked-multiply
                                       util/phi-1 (unchecked-subtract max-idx min-idx)))))
            item (.nth coll idx)]
        (if (< 0 (.compare cmptor item k))
          (recur min-idx idx)
          (recur (unchecked-inc idx) max-idx))))))

(comment
  (let [lst (List/from [0 1 2 3 4 6 6 6 6 9 10])]
    (and (= 9 (bcoll-search-insertion-idx compare lst 6))
         (= 4 (bcoll-search-insertion-idx compare lst 3))
         (= 0 (bcoll-search-insertion-idx compare lst -1))
         (= 11 (bcoll-search-insertion-idx compare lst 11))))

;; use chunking for subview, but only draw visible
  ;; for now, all file items in memory
  (def --comparator)
  (definterface IReplClass
    (reset [paths])
    (create [^java.nio.file.Path path])
    (modify [^java.nio.file.Path path])
    (delete [^java.nio.file.Path path]))
  (deftype ReplClass [^:unsynchronized-mutable ^List file-names
                      ^:unsynchronized-mutable ^List cmpts
                      ^int cmpt-offset]
    clojure.lang.ILookup
    (valAt [self k] (.valAt self k nil))
    (valAt [_ k nf]
      (case k
        :file-names file-names
        :cmpts cmpts
        nf))

    IReplClass
    (reset [_ paths]
      (let [file-names-tmp (.linear List/EMPTY)
            a (to-array paths)]
        (java.util.Arrays/sort a)
        (doary [p a]
          (.addLast file-names-tmp (fs/file-name p)))
        (set! file-names (.forked file-names-tmp))))

    (create [_ path]
      (let [fname (fs/file-name path)
            insert-idx (bcoll-search-insertion-idx
                        (java.util.Comparator/naturalOrder) file-names fname)]
        (set! file-names
              (-> (.slice file-names 0 insert-idx)
                  (.addLast fname)
                  (.concat (.slice file-names insert-idx (.size file-names)))))))

    (modify [_ path])

    (delete [_ path]
      (let [fname (fs/file-name path)
            idx (bcoll-search-insertion-idx
                 (java.util.Comparator/naturalOrder) file-names fname)
            size (.size file-names)]
        (set! file-names
              (-> (.slice file-names 0 idx)
                  (cond-> (< idx size)
                    (.concat (.slice file-names (unchecked-inc idx) size))))))))

  (def --thing (->ReplClass List/EMPTY List/EMPTY 0))
  (.create --thing (nth (fs/list-dir ".") 0))
  (.reset --thing (take 3 (fs/list-dir ".")))
  (doseq [p (drop 3 (fs/list-dir "."))]
    (.create --thing p))

  (seq (:file-names --thing))

  (sort (fs/list-dir "."))

  (let [lst (List/from [0 1 2 3 4])
        lst2 (.slice lst 0 2)]
    (.removeLast
     (.concat (.addLast lst2 "x")
              (.slice lst 2 5))))

  ;; (fs/list-dir (io/file "."))

  #_(with-open [ds (Files/newDirectoryStream (fs/path "."))]
      (mapv str ds))

#!
  )
