(ns chic.main
  (:require
    [taoensso.encore :as enc]
    [babashka.fs :as fs]
    [clojure.main]
    [nrepl.core :as nrepl]
    [nrepl.cmdline]
    [nrepl.server]
    [chic.debug.nrepl :as debug.nrepl]
    [rebel-readline.clojure.line-reader]
    [rebel-readline.clojure.main]
    [rebel-readline.clojure.service.local]
    [rebel-readline.core]))

;; (set! *warn-on-reflection* true)

(def nrepl-server nil)
(def nrepl-transport nil)

(defn start-nrepl-server! []
  (alter-var-root #'nrepl-server
    (fn [x]
      (when (some? x) (nrepl.server/stop-server x))
      (let [server (nrepl.server/start-server
                     ;:port 7888
                     {})]     
        (try
          (alter-var-root #'nrepl-transport
            (constantly (nrepl/connect :port (:port server))))
          (debug.nrepl/add-middleware (nrepl/client nrepl-transport 1000))
          (nrepl.cmdline/save-port-file server {})
          server
          (catch Throwable _
            (nrepl.server/stop-server server)
            nil))))))

(defn start-rebel-readline! []
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
      (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
      :prompt (fn [])
      :read (rebel-readline.clojure.main/create-repl-read))))

(def *threads (atom {}))

(defn on-shutdown []
  (fs/delete-if-exists ".nrepl-port"))

(defn start-app! []
  (enc/reset-val! *threads "app-start"
    (-> (Thread/ofVirtual)
      (.name "chic.main app start")
      (.start (requiring-resolve 'chic.uiroot/start-ui)))))

(defn -main [& args]
  (start-nrepl-server!)
  (enc/reset-val! *threads "rebel-readline"
    (-> (Thread/ofVirtual)
      (.name "chic.main rebel-readline")
      (.start start-rebel-readline!)))
  (.addShutdownHook (Runtime/getRuntime)
    (Thread. #'on-shutdown))
  (when (not-any? #{"nostart"} args) (start-app!)))

(comment
  (start-app!)
  (start-nrepl-server!)
  (debug.nrepl/add-middleware (nrepl/client nrepl-transport 1000))
  
  ;(defn nrepl-client (nrepl/client nrepl-transport 1000))

  
  
  )
