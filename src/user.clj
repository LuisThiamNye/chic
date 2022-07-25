(ns user)

;;(require '[nrepl.server :as server] '[rebel-readline.core] '[rebel-readline.clojure.line-reader]   '[rebel-readline.clojure.service.local]   '[rebel-readline.clojure.main] '[cider.nrepl :refer [cider-nrepl-handler]])

;(defn -main [& args])

(comment
  (require '[lambdaisland.classpath.watch-deps :as watch-deps])
  (watch-deps/start! {:aliases [:dev]})
  
  (require '[clojure.tools.deps.alpha.repl])
  (clojure.tools.deps.alpha.repl/add-libs
    '{philoskim/debux {:mvn/version "0.8.2"}
      com.lambdaisland/classpath {:mvn/version "0.0.27"}})
  
  (require 'debux.core)
  )