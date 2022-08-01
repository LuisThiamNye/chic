(ns chic.digger2.inspector
  (:require
    [potemkin :refer [doit]]
    [chic.util :as util :refer [doit-zip]]
    [chic.ui2.event :as ievt]
    [chic.style :as style]
    [chic.ui.canvas :as cnv]
    [chic.paint :as cpaint]
    [chic.ui.font :as uifont]
    [chic.digger2.inspector.obj-view :as obj-view]
    [chic.ui3.interactor :as uii3]
    [chic.clj-editor2.file-tree.ui-interact :as ft.ui-interact]
    [chic.ui.ui3 :as ui3])
  (:import
    (io.github.humbleui.skija Font)
    (io.github.humbleui.types Rect)
    (java.lang AutoCloseable)))

(ui3/deffnletcmpt ui-titlebar 
  [intrmgr ^Rect rect ^float scale object]
  (let [bg-paint (cpaint/fill 0xFFd0d0d0)
        text-paint (cpaint/fill 0xFF000000)
        cap-height (* 0.5 (:height rect))
        text-y (/ (+ (:bottom rect) (:y rect) cap-height) 2)
        font (uifont/caph->font style/face-default cap-height)
        text-line (uifont/shape-line-default font (str object))
        text-x (+ (:x rect) (* 0.2 (:height rect)))]
    {:draw
     (fn [cnv]
       (.drawRect cnv rect bg-paint)
       (cnv/with-save cnv
         (.clipRect cnv rect)
         (.drawTextLine cnv text-line text-x text-y text-paint)))}))

(ui3/deffnletcmpt ui-inspector-content
  [intrmgr ^Rect rect ^float scale object]
  (let [cmpt-titlebar (ui3/new-cmpt ui-titlebar)
        titlebar-rect (.withHeight rect (* scale 20))
        view-rect (assoc rect :y (:bottom titlebar-rect))
        cmpt-obj-view (ui3/new-cmpt obj-view/ui-obj-view)]
    {:draw
     (fn [cnv]
       (ui3/draw-cmpt cmpt-titlebar cnv
         {:scale scale
          :intrmgr intrmgr
          :object object
          :rect titlebar-rect})
       (ui3/draw-cmpt cmpt-obj-view cnv
         {:scale scale :intrmgr intrmgr :object object
          :rect view-rect}))}))

(ui3/deffnletcmpt ui-inspector-flow
  [intrmgr ^Rect rect ^float scale]
  (let [intr (uii3/open-interactor intrmgr {})
        *content-rect (volatile! (Rect. 0 0 0 0))
        _ (ft.ui-interact/scroll-fill-rect rect @*content-rect)
        _ (uii3/refresh-intr
            intr
            {:rect rect
             :on-scroll
             (fn [_ _ evt]
               (vreset! *content-rect
                 (ft.ui-interact/scroll-rect
                   rect @*content-rect
                   (ievt/scroll-dx evt) (ievt/scroll-dy evt))))})
        ^:always ^:diff= children [{:rect (Rect. 0 0 300 500)
                                   :object {:x 1 :y 2 :z {0 [] 2 [1 2]}}}]
        first-child-idx 0
        children-cmpts [(ui3/new-cmpt ui-inspector-content)]
        _close (reify java.lang.AutoCloseable
                 (close [_] (doit [^AutoCloseable c children-cmpts]
                              (.close c))))]
    {:draw
     (fn [cnv]
       (doit-zip [cmpt children-cmpts
                  i (util/int-range-it first-child-idx Integer/MAX_VALUE)]
         (let [child (nth children i)]
           (ui3/draw-cmpt ^{:cmpt ui-inspector-content} cmpt cnv
             {:intrmgr intrmgr
              :scale scale
              :rect (do children (:rect child))
              :object (do children (:object child))}))))}))
