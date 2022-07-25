(ns chic.clj-editor2.file-tree
  (:require
   [babashka.fs :as fs]
   [chic.debug :as debug]
   [chic.ui3.interactor :as uii3]
   [clojure.walk :as walk]
   [clojure.java.io :as io]
   [clojure.math :as math]
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.ui.ui2 :as ui2]
   [chic.ui2.event :as ievt]
   [chic.ui.ui3 :as ui3]
   [chic.ui.icons.material :as maticons]
   [chic.clj-editor2.file-tree.ui-interact :as ui-interact]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.util.inline :refer [inline-fnsnip-multiretmap
                             swap-let-binding-input-syms
                             fnlet-snippet]]
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

(do
  (def --code1
    (util/quoted
     (ui3/fnlet-widget
      (fn ui-icon-and-label
        [centre-y width->xbounds height text-width
         textblob text-paint cap-height wh->icon-image]
        (let [baseline-y (+ centre-y (/ cap-height 2))
              icon-length (min height (* 1.2 cap-height))
              icon-image ^Image (wh->icon-image icon-length icon-length)
              icon-rx (/ (- height icon-length) 2)
              text-rx (+ icon-rx (* util/phi icon-length))
              xbounds (width->xbounds (+ text-rx text-width))
              icon-image-rect
              (let [iheight (.getHeight icon-image)
                    iwidth (.getWidth icon-image)]
                (Rect/makeXYWH (+ (:x xbounds) icon-rx) (- centre-y (/ iheight 2))
                               iwidth iheight))]
          {:draw
           (fn [cnv]
             (.drawImageRect cnv icon-image icon-image-rect)
             (.drawTextBlob cnv textblob (+ (:x xbounds) text-rx) baseline-y text-paint))})))))
  (def ui-icon-and-label (eval --code1)))

(comment
  (binding [*print-meta* true])
  #!
  )

(def ui-icon-and-label-apt
  (ui3/fnlet-widget
   (fn ui-icon-and-label-apt
     [idx filename cap-height item-height content-y widthch->xbounds text-paint font]
     (let [[textblob text-width] (let [textline (uifont/shape-line-default font filename)]
                                   [(.getTextBlob textline) (.getWidth textline)])
           *prev-width (volatile! 0)
           cmpt (ui3/new-cmpt ui-icon-and-label)]
       {:draw
        (fn [cnv]
          ;; (prn "ilapt: in" (ui3/get-input-chmask) (ui3/get-changed-input-syms))
          (ui3/draw-cmpt
           cmpt cnv
           {:centre-y (+ content-y (* (+ 0.5 idx) item-height))
            :text-width text-width
            :width->xbounds (fn -width->xbounds [width]
                              (let [prev @*prev-width]
                                (vreset! *prev-width width)
                                (widthch->xbounds prev width)))
            :height item-height
            :textblob textblob
            :text-paint text-paint
            :cap-height cap-height
            :wh->icon-image
            (fn [^long w ^long h]
              widthch->xbounds
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
      (fn ui-linear-treeview
        [width->xbounds height->ybounds ^Rect frame-rect ^Rect visible-rect ^float scale mouse-pos
         children-data ^int item-height intrmgr]
        (let [ybounds (height->ybounds (* item-height (count children-data)))
              content-y (:y ybounds)
              #_#_{:keys [first-visible-idx last-visible-idxe content-x content-y]}
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
              ui-children (mapv (fn [_] (ui3/new-cmpt ui-icon-and-label-apt))
                                children-data)
              group-ranges (children-data->group-ranges children-data)
              vline-cmpts (mapv (fn [_] (ui3/new-cmpt ui-indent-vline))
                                group-ranges)
              intr (uii3/open-interactor intrmgr {})
              hover-idx (when (util/<=< (:x visible-rect) (:x mouse-pos) (:right visible-rect))
                          (let [idx (long (Math/floor (/ (- (:y mouse-pos) content-y) item-height)))]
                            (when (util/<=< 0 idx (count children-data))
                              idx)))
              visible-hover-rect (when hover-idx
                                   (Rect. (:x visible-rect) (+ content-y (* hover-idx item-height))
                                          (:right visible-rect) (+ content-y (* (inc hover-idx) item-height))))
              hover-paint (huipaint/fill 0x10000000)
              *child-widths (volatile! (sorted-set))
              *xbounds (volatile! {:x 0 :right 0})
              ^:always ^:diff xbounds @*xbounds
              _ (when-some [width (first (rseq @*child-widths))]
                  (vreset! *xbounds (width->xbounds width)))]
          {:draw
           (fn [cnv]
             (when hover-idx
               (.drawRect cnv visible-hover-rect hover-paint))
             ;; (prn "treeview: in" (ui3/get-input-chmask) (ui3/get-changed-input-syms))
             ;; (prn "treeview: f" (ui3/get-field-chmask) (ui3/get-changed-field-syms))

             (doit-zip [{:keys [level filename]} (eduction children-data)
                        i (util/int-range-it)]
               (ui3/draw-cmpt ^{:cmpt ui-icon-and-label-apt} (nth ui-children i)
                              cnv
                              {:idx (do children-data i)
                               :filename (do children-data filename)
                               :cap-height cap-height
                               :item-height item-height
                               :content-y content-y
                               :widthch->xbounds
                               (fn -widthch->xbounds [prev-width width]
                                 (let [xindent (* scale 2 level)
                                       full-prev-width (+ xindent prev-width)
                                       full-width (+ xindent width)
                                       max-width1 (first (rseq @*child-widths))
                                       child-widths (vswap! *child-widths #(-> % (disj full-prev-width)
                                                                               (conj full-width)))
                                       max-width2 (first (rseq child-widths))
                                       {:keys [x right]} (if (= max-width1 max-width2)
                                                       @*xbounds
                                                       (vreset! *xbounds (width->xbounds max-width2)))]
                                   {:x (+ xindent x) :right right}))
                               :text-paint text-paint
                               :font font}))
             (doit-zip [[start ende] ^Iterable group-ranges
                        cmpt ^Iterable vline-cmpts]
               (ui3/draw-cmpt ^{:cmpt ui-indent-vline} cmpt cnv
                              {:scale scale
                               :content-x (max (:x xbounds) (:x frame-rect))
                               :level (:level (nth children-data start))
                               :first-child-top (+ content-y (* item-height (inc start)))
                               :parent-top (+ content-y (* item-height start))
                               :last-child-bottom (+ content-y (* item-height ende))}))
             (when (not= xbounds @*xbounds)
               (uii3/refresh-intr intr {:rect (Rect. (:x @*xbounds) content-y
                                                     (:right @*xbounds) (:bottom ybounds))
                                        :on-mousedown (fn [_ _ctx evt]
                                                        (prn 'hi))})))})))))
  (def ui-linear-treeview (eval --tree-code)))

(comment
  (binding [*print-meta* true]
    (debug/puget-prn
     (macroexpand --tree-code)))
  #!
  )

(def sample-children-data
  (let [expanded-dirs #{(str (fs/canonicalize (fs/path "src")))
                        (str (fs/canonicalize (fs/path "src/chic")))}]
    (letfn [(todir [path level]
              (with-open [ds (Files/newDirectoryStream path)]
                (let [it (util/to-iter
                          (sort (filterv (fn [p] (not (re-find #"^\.#|.~undo-tree~$" (fs/file-name p)))) ds)))]
                  (loop [dir []]
                    (if-some [path (util/iter-next it)]
                      (let [fname (fs/file-name path)]
                        (recur (cond-> (conj dir {:filename fname :level level})
                                 (expanded-dirs (str (fs/canonicalize path)))
                                 (into (todir path (inc level))))))
                      dir)))))]
      (todir (fs/path ".") 0))))

(do
  (def --dev-view-code
    (util/quoted
     (ui3/fnlet-widget
      (fn ui-dev-view [intrmgr ^float scale rect visible-rect]
        (let [bg-paint (huipaint/fill (unchecked-int 0xFFeff2f7))
              ui-list (ui3/new-cmpt ui-linear-treeview)
              *init? (volatile! true)
              item-height (Integer. (int (* scale 20)))
              children-data sample-children-data
              *content-rect (volatile! (Rect. 0 0 0 0))
              _ (ui-interact/scroll-fill-rect rect @*content-rect)
              ^:always ^:diff= content-rect @*content-rect
              scroll-intr (uii3/open-interactor intrmgr {})
              _ (uii3/refresh-intr
                 scroll-intr
                 {:rect rect
                  :on-scroll
                  (fn [_ _ evt]
                    (vreset! *content-rect
                             (ui-interact/scroll-rect
                              rect @*content-rect
                              (ievt/scroll-dx evt) (ievt/scroll-dy evt))))})
              size->rect (fn [width height]
                           (vreset! *content-rect
                                    (ui-interact/scroll-fill-rect
                                     rect
                                     (Rect/makeXYWH (:x @*content-rect)
                                                    (:y @*content-rect)
                                                    width height))))
              ^:always ^:diff mouse-pos (ui3/get-mouse-pos)]
          {:draw (fn [cnv]
                   (.drawRect cnv visible-rect bg-paint)
                   ;; (prn "dev: in: "(ui3/get-input-chmask) (ui3/get-changed-input-syms))
                   ;; (prn "dev: f: " (ui3/get-field-chmask) (ui3/get-changed-field-syms))
                   (ui3/draw-cmpt
                    ui-list cnv
                    {:-init? @*init?
                     :intrmgr intrmgr
                     :width->xbounds (fn [width] content-rect
                                       (size->rect (+ width (* 10 scale)) (:height @*content-rect)))
                     :height->ybounds (fn [height] content-rect
                                        (size->rect (:width @*content-rect) height))
                     :visible-rect visible-rect
                     :frame-rect rect
                     :mouse-pos mouse-pos
                     :scale scale
                     :children-data children-data
                     :item-height item-height})
                   (when @*init? (vreset! *init? false)))})))))
  (def ui-dev-view (eval --dev-view-code)))

(comment
  (binding [*print-meta* true]
    (debug/puget-prn
     (clojure.tools.analyzer.jvm/macroexpand-1 --dev-view-code)))
  #_(def --data (last(:draw-cmpt (debug/with-capture-data
                                 (try (eval --dev-view-code)
                                      (catch Exception _))))))
  #!
  )

(do
  (def --a-view-code
    (util/quoted
     (let [intrmgr (uii3/new-manager)
           ui-root (ui3/new-cmpt ui-dev-view)
           input-memory (ui3/cmpt-ext-input-memory ui-root)]
       (ui2/on-mount
        (fn [_ctx])
        (fn close [_]
          (.close ^java.lang.AutoCloseable ui-root))
        (ui2/on-event
         (fn [ctx evt]
           (uii3/-handle-jwm-event intrmgr ctx evt))
         ((ui2/direct-widget
           {:draw (fn [_ {:keys [scale]} rect ^Canvas cnv]
                    (ui3/draw-cmpt-ext-memo
                     ui-root cnv input-memory
                     {:rect ^:diff= rect
                      :visible-rect ^:diff= rect
                      :scale scale
                      :intrmgr intrmgr}))}) {}))))))
  (defn a-view []
    (util/compile --a-view-code)))

(comment
  (windows/remount-all-windows)
  (debug/puget-prn
   (clojure.tools.analyzer.jvm/macroexpand-all --a-view-code))
  (= --x1 (aget (object-array [(unchecked-float 1.0)]) 0))
  (:input-syms ui-dev-view)

  ;; detect Closeables
  #!
  )
