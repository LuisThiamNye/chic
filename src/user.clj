(ns user
  #_(:require
    [debux.core]))

;(defn -main [& args])
(comment
  (set! *print-length* 2000)
  (set! *print-level* 8)
  (set! *warn-on-reflection* true))

(comment
  (require '[lambdaisland.classpath.watch-deps :as watch-deps])
  
  (defonce --watcher
    (watch-deps/start! {:aliases [:dev]}))
  
  (require '[clojure.tools.deps.alpha.repl])
  (clojure.tools.deps.alpha.repl/add-libs
    '{})
  
  (run! prn (sort (map #(.getName %) (.keySet (Thread/getAllStackTraces)))))
  )
