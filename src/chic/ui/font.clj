(ns chic.ui.font
  (:require
   [taoensso.encore :as enc]
   [chic.ui :as cui])
  (:import
   (io.github.humbleui.skija TextLine Font Typeface)
   (io.github.humbleui.skija.shaper ShapingOptions)))

(defn ^TextLine shape-line-default [font text]
  (.shapeLine cui/shaper text font ShapingOptions/DEFAULT))

(defn calc-size-caph-ratio [^Typeface typeface]
  (let [s 20.]
    (/ s (.getCapHeight (.getMetrics (Font. typeface (float s)))))))

(def ^java.util.concurrent.ConcurrentHashMap
  -caph-size-cache (java.util.concurrent.ConcurrentHashMap.))

(defn caph->size ^double [^Typeface typeface cap-height]
  (let [id (.getUniqueId typeface)
        cs-ratio (unchecked-float
                  (or (.get -caph-size-cache id)
                      (let [v (unchecked-float (calc-size-caph-ratio typeface))]
                        (.put -caph-size-cache id v)
                        v)))]
    (unchecked-multiply (unchecked-float cap-height) cs-ratio)))
