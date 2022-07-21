(ns chic.clj-editor2.file-tree.ui-interact
  (:require
   [taoensso.encore :as enc]
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
   [chic.util.inline]
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

#_(do
    (def --mouse-highlight-code
      (ui3/fnlet-widget
       (fn ui-mouse-highlight
         [mouse-pos]
         (let []
           {:draw (fn [cnv])}))))
    (def ui-mouse-highlight (eval --mouse-highlight-code)))

#_(defn calc-scroll-offset [prev-offset-x prev-offset-y content-rect dx dy]
    {:offset-x (min (max (- (:x content-rect) (:right content-rect))
                         (+ (or prev-offset-x 0) dx)) 0)
     :offset-y (min (max (- (:y content-rect) (:bototm content-rect))
                         (+ prev-offset-y dy)) 0)})
(defn scroll-rect ^Rect [^Rect frame-rect ^Rect content-rect dx dy]
  (.offset content-rect
           (if (< 0 dx)
             (enc/clamp* 0 dx (- (:x frame-rect) (:x content-rect)))
             (enc/clamp* dx 0 (- (:right frame-rect) (:right content-rect))))
           (if (< 0 dy)
             (enc/clamp* 0 dy (- (:y frame-rect) (:y content-rect)))
             (enc/clamp* dy 0 (- (:bottom frame-rect) (:bottom content-rect))))))
(comment
  (= (Rect. -10 -10 10 10)
     (scroll-rect (Rect. 0 0 10 10) (Rect. 0 0 20 20) -40 -40))
  (= (Rect. 0 0 10 10)
     (scroll-rect (Rect. 0 0 20 20) (Rect. 0 0 10 10) 1 1))
  (= (Rect. 0 0 10 10)
     (scroll-rect (Rect. 0 0 20 20) (Rect. 0 0 10 10) -1 -1)))

(defn scroll-fill-rect ^Rect [^Rect frame-rect ^Rect content-rect]
  (.offset content-rect
           (let [excess (- (:right frame-rect) (:right content-rect))
                 budget (- (:x frame-rect) (:x content-rect))]
             (max 0 (min excess budget)))
           (let [excess (- (:bottom frame-rect) (:bottom content-rect))
                 budget (- (:y frame-rect) (:y content-rect))]
             (max 0 (min budget excess)))))
(comment
  (= (Rect. 0 0 20 20)
     (scroll-fill-rect (Rect. 0 0 10 10) (Rect. 0 0 20 20)))
  (= (Rect. 0 0 10 10)
     (scroll-fill-rect (Rect. 0 0 20 20) (Rect. 0 0 10 10)))
  (= (Rect. 0 0 10 10)
     (scroll-fill-rect (Rect. 0 0 20 20) (Rect. -10 -10 0 0)))
  (= (Rect. -10 -10 20 20)
     (scroll-fill-rect (Rect. 0 0 10 10) (Rect. -10 -10 20 20))))

#_(do
    (def --mouse-highlight-code
      (ui3/fnlet-widget
       (fn ui-mouse-highlight
         [mouse-pos]
         (let [^:mut offest-x 0
               ^:mut offset-y 0
               scroll-listener (ui3/open-subscribe! :mouse-scroll
                                                    (fn [_]))]
           {:draw (fn [cnv])}))))
    (def ui-mouse-highlight (eval --mouse-highlight-code)))

(comment
  (def simple-scroll-state
    (ui3/fnlet-widget
     (fn simple-scroll-state [intrmgr rect content-rect]
       (let [^:mut offset-x 0
             ^:mut offset-y 0
             interactor (ui3/attached-interactor intrmgr {})
             _ (ui3/configure-interactor
                interactor
                {:rect rect
                 :on-scroll (fn [_ evt]
                              (let [r (calc-scroll-offset offset-x offset-y content-rect
                                                          (scroll-dx evt) (scroll-dy evt))]
                                (set! offset-x (:offset-x r))
                                (set! offset-y (:offset-y r))))})]
         {:expose [offset-x offset-y]}))))

  (deftype SimpleScrollState [^:unsynchronized-mutable offset-x
                              ^:unsynchronized-mutable offset-y
                              interactor]
    :keys [offset-x offset-y])

  (defn open-simple-scroll []
    ())

  (defn refresh-simple-scroll [state rect content-rect]))
