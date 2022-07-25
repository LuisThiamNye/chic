(ns dev 
  (:require
    [nrepl.cmdline :as nreplcmd]))

(require '[nrepl.cmdline :as nreplcmd] '[nrepl.server :as server] '[rebel-readline.core] '[rebel-readline.clojure.line-reader]   '[rebel-readline.clojure.service.local]   '[rebel-readline.clojure.main] '[cider.nrepl :refer [cider-nrepl-handler]])

(def server
  (let [r (promise)]
    (.start (Thread. 
              (fn [] (deliver r
                       (server/start-server
                         :port 7890
                         {:handler cider-nrepl-handler}))))) r))

(defn start-rebel []
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
      (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
      :prompt (fn [])
      :read (rebel-readline.clojure.main/create-repl-read))))

(def rebel
  (let [r (promise)]
    (.start
      (Thread.
        (fn []
          (deliver r (start-rebel)))))
    r))
    
(defn -main [& args])
