(ns chic.digger2.inspector.obj-view
  (:require
    [potemkin :refer [doit doary]]
    [chic.util :as util :refer [doit-zip loop-zip loopr <-]]
    [chic.ui2.event :as ievt]
    [chic.style :as style]
    [chic.ui.canvas :as cnv]
    [chic.paint :as cpaint]
    [chic.ui.font :as uifont]
    [chic.ui3.interactor :as uii3]
    [chic.clj-editor2.file-tree.ui-interact :as ft.ui-interact]
    [chic.ui.ui3 :as ui3])
  (:import
    (java.lang.reflect Field Modifier)
    (io.github.humbleui.skija Font)
    (io.github.humbleui.types Rect)
    (java.lang AutoCloseable)))

(ui3/deffnletcmpt ui-field-line
  [^float scale intrmgr ybounds width->xbounds object ^Field field ^Font font]
  (let [title-line (let [mods (.getModifiers field)]
                     (uifont/shape-line-default font 
                       (str (cond (Modifier/isPrivate mods) "-P "
                              (Modifier/isProtected mods) "-pr ")
                         (when (Modifier/isStatic mods) "-S ")
                         (cond (Modifier/isVolatile mods) "-v "
                           (Modifier/isFinal mods) "-F ")
                         (when (Modifier/isSynchronized mods) "-sync ")
                         (when (Modifier/isTransient mods) "-t ")
                         (.getName field) " = " (or (.get field object) "null"))))
        xbounds (width->xbounds 0)
        title-paint (cpaint/fill 0xFF000000)]
    {:draw
     (fn [cnv]
       (.drawTextLine cnv title-line (:x xbounds) (:bottom ybounds) title-paint))}))

(defn field->comparable [^Field f]
  (let [mods (.getModifiers f)
        vis (cond (Modifier/isPrivate mods) 2
              (Modifier/isProtected mods) 1
              :else 0)
        mut (cond (Modifier/isVolatile mods) 1
              (Modifier/isFinal mods) 0
              :else 2)]
    (vector vis (not (Modifier/isStatic mods))
      mut
      (not (Modifier/isSynchronized mods))
      (Modifier/isTransient mods)
      (.getName f))))

(ui3/deffnletcmpt ui-fields-view
  [^float scale intrmgr ^Rect rect object]
  (let [line-height (* 16 scale)
        font (uifont/caph->font style/face-default (* line-height 0.6))
        fields (.getDeclaredFields (.getClass object))
        _ (java.util.Arrays/sort fields
            (fn [^Field a ^Field b]
              (compare (field->comparable a) (field->comparable b))))
        _ (doary [^Field f fields] (.setAccessible f true))
        field-cmpts (mapv (fn [_] (ui3/new-cmpt ui-field-line)) fields)
        _ (ui3/coll-closer field-cmpts)
        line-w->xb (fn [_w] rect)]
    {:draw
     (fn [cnv]
       (loop-zip [cmpt field-cmpts
                  field ^objects fields]
         [ybounds (.withHeight rect line-height)]
         (do (ui3/draw-cmpt ^{:cmpt ui-field-line} cmpt cnv
               {:scale scale :intrmgr intrmgr 
                :object object 
                :ybounds (do rect line-height ybounds)
                :width->xbounds line-w->xb :font font
                :field (do fields field)})
           (recur (.offset ybounds 0 line-height)))))}))

(comment
  (-> (ui3/fnlet-widget-parse*
        (last (last (macroexpand-1 dev/last-code))))
    ; :bindings-anas (nth 1) :tag
    :draw-env :locals (get 'fields) keys)
  
  (-> --x (get 'fields)
    :init keys)
  )

(ui3/deffnletcmpt ui-obj-view
  [^float scale intrmgr ^Rect rect object]
  (let [large-font (uifont/caph->font style/face-default (* scale 12))
        small-font (uifont/caph->font style/face-default (* scale 9))
        clsns-textline (uifont/shape-line-default small-font 
                         (second (re-find #"^(.+)\.[^.]+$" (.getName (class object)))))
        class-textline (uifont/shape-line-default large-font 
                         (second (re-find #"([^.]+)$" (.getName (class object)))))
        text-paint (cpaint/fill 0xFF000000)
        flds-cmpt (ui3/new-cmpt ui-fields-view)
        bg-paint (cpaint/fill 0xFFffffff)]
    {:draw
     (fn [cnv]
       (.drawRect cnv rect bg-paint)
       (let [ns-y (+ (* scale 12) (:y rect))
             cls-y (+ ns-y (* 17 scale))]
         (.drawTextLine cnv clsns-textline (:x rect) ns-y text-paint)
         (.drawTextLine cnv class-textline (:x rect) cls-y text-paint))
       (ui3/draw-cmpt flds-cmpt cnv
         {:scale scale :intrmgr intrmgr :object object
          :rect (.offset rect 0 (* scale 40))}))}))
