(ns chic.ui.canvas
  (:import
    (io.github.humbleui.skija Canvas)))

(defmacro with-save [canvas & body]
  (assert (symbol? canvas))
  `(let [c# (.save ^Canvas ~canvas)]
     (try
       ~@body
       (finally (.restoreToCount ^Canvas ~canvas c#)))))