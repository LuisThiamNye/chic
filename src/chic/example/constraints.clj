(ns chic.example.constraints
  (:require
   [taoensso.encore :as enc]
   [io.github.humbleui.paint :as huipaint]
   [chic.util :as util]
   [chic.ui.ui2 :as ui2]
   [chic.ui :as cui]
   [clojure.pprint :as pp]
   [proteus :refer [let-mutable]]
   [potemkin :refer [doit]]
   [clj-commons.primitive-math :as prim]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   ;; [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui])
  (:import
   (java.util ArrayList)
   (io.github.humbleui.skija Canvas Font Paint TextLine FontMetrics)
   (io.github.humbleui.skija.shaper ShapingOptions Shaper)
   (io.github.humbleui.types IPoint IRect Rect Point)
   (java.lang AutoCloseable)))

;; toolbar and tabs

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment

  (defn button []
    {:state [state text font]
     :constraints-changed
     (fn [crect])
     :setters {:text (fn [prev now])}
     :draw
     (fn [rect])})

  ;; based on rects
  (defn main [win]
    (let [c-toolbar (subrect win :top :hstretch)
          c-tabbar (subrect win :hstretch {:below toolbar})
          c-button1 (rect)
          c-button2 (rect)
          c-buttons (rect)
          c-title (rect)
          _ (split-row c-toolbar [(split-stretch c-title) c-buttons (gap 5)])
          _ (split-row c-buttons [c-button1 (gap 5) c-button2])
          button1 ($ button c-button1)
          button2 ($ button c-button2)]
      [($ fill c-toolbar :grey)
       ($ title c-title)
       button1
       button2
       ($ tabbar c-tabbar)]))

  ;; what about based on lines?
  ;; or both? - rects are useful when inserting between components
  ;;      when the overlapping lines separate

  ;; tree structure would be useful for ownership and managing lifecycles of components.
  ;; Constraints can be applied separately to the tree. Tree does not have to be coupled
  ;; to the visuals.

  (defn main [win]
    (vsplit
     (hsplit
      ($ title)
      (hsplit
       ($ button)
       (gap 5)
       ($ button))
      (gap 5))
     ($ tabbar))))

;; translating/resizing button

(defn view1 []
  (let [pressed? (atom false)
        *click-area (atom (Rect. 0 0 0 0))
        ui (ui2/stack
            (ui2/fill-rect (huipaint/fill 0xffeffafc))
            (ui2/adapt-rect
             (fn [_ rect]
               (reset! *click-area
                       (ui2/-rect-haligned
                        (if @pressed?
                          1 0) 0.5
                        rect
                        (Rect. 0. 0. 100. 100.))))
             (ui2/fill-rect (huipaint/fill 0x7f000000))))
        on-mount (fn [_ctx])
        *unmounted? (volatile! true)]
    (cui/generic
     {:draw
      (fn [_ ctx ^IRect rect ^Canvas cnv]
        (let [layer (.save cnv)]
          (.translate cnv (- (:x rect)) (- (:y rect)))
          (when @*unmounted?
            (on-mount ctx)
            (vreset! *unmounted? false))
          (ui2/draw ui ctx (.toRect rect) cnv)
          (.restoreToCount cnv layer)))
      :event (fn [_ evt]
               (if (and (:hui.event.mouse-button/is-pressed evt)
                        (cui/point-in-component? evt (:chic.ui/mouse-win-pos evt)))
                 (when (.contains ^Rect @*click-area (.toPoint ^IPoint (:chic.ui/mouse-win-pos evt)))
                   (reset! pressed? true))
                 (reset! pressed? false)))
      :close (fn [_])})))
