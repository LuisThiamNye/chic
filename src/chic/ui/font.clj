(ns chic.ui.font
  (:require
   [chic.ui :as cui])
  (:import
   (io.github.humbleui.skija TextLine)
   (io.github.humbleui.skija.shaper ShapingOptions)))

(defn ^TextLine shape-line-default [font text]
  (.shapeLine cui/shaper text font ShapingOptions/DEFAULT))
