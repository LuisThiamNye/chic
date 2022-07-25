(ns chic.main
  (:require
    [taoensso.encore :as enc]
   [babashka.fs :as fs]
   [borkdude.dynaload :refer [dynaload]]
   [clojure.main]
   [nrepl.cmdline :as nrepl.cmdline]
   [nrepl.server :as nrepl.server]
   [rebel-readline.clojure.line-reader]
   [rebel-readline.clojure.main]
   [rebel-readline.clojure.service.local]
   [rebel-readline.core]))

(def *cider-nrepl-handler (dynaload 'cider.nrepl/cider-nrepl-handler {:default nil}))

;; (set! *warn-on-reflection* true)

(require '[chic.debug.swap :as debug.swap])
;; (debug.swap/install-all-instrumentation!)

(require '[chic.debug :as debug] )
;; (debug.swap/install-all-instrumentation!)

(def nrepl-server nil)

(defn start-nrepl-server! []
  (alter-var-root #'nrepl-server
    (fn [x]
      (when (some? x) (nrepl.server/stop-server x))
      (doto (nrepl.server/start-server
              ;:port 7888
              (if-some [cider-nrepl-handler @*cider-nrepl-handler]
                {:handler cider-nrepl-handler}
                {}))
        (nrepl.cmdline/save-port-file {})))))

(defn start-rebel-readline! []
  (rebel-readline.core/with-line-reader
    (rebel-readline.clojure.line-reader/create
      (rebel-readline.clojure.service.local/create))
    (clojure.main/repl
      :prompt (fn [])
      :read (rebel-readline.clojure.main/create-repl-read))))

(def *threads (atom {}))

(defn -main [& _args]
    (-> (Thread/ofPlatform) 
      (.name "chic.main nrepl server")
      .daemon
      (.start start-nrepl-server!))
  (enc/reset-val! *threads :rebel-readline
    (-> (Thread/ofPlatform)
      (.name "chic.main rebel readline")
      .daemon
      (.start start-rebel-readline!)))
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. (fn [] (fs/delete-if-exists ".nrepl-port"))))
  ;; (debug/attach-vm!)
  ((requiring-resolve 'chic.uiroot/start-ui))
  #_(.join ^Thread (:rebel-readline @*threads)))

(comment
  (windows/dosendui
   (make-main-window))
  (do
    (hui/doui
     (some-> (some #(when (= "main" (:id %)) %) (vals @windows/*windows))
             :window-obj huiwin/close)
     (make-main-window)))
  (hui/doui-async (/ 0))
  (hui/doui (.getUncaughtExceptionHandler (Thread/currentThread)))

  (def x (hui/doui-async (chic.windows/request-frame (first (vals @windows/*windows)))))
  (def x (hui/doui-async 5))

  @(:*ctx (first (vals @windows/*windows)))
  (:*ui-error (second (vals @windows/*windows)))
  (@*app-root)
  (io.github.humbleui.protocols/-draw (build-app-root))

  (System/gc)
  ;; a single scope/chain for shortcuts/key events where there can only be one handler at most.
  ;; widgets register/deregister their shortcuts as they come into/out of focus
  ;;   or reg/dereg upon mount/unmount and pred includes whether it is focussed.

  (.write (get @(:session (val (first @cider.nrepl.middleware.out/tracked-sessions-map)))
               #'*out*)
          "xyz")
  (.println (System/out) "x")
  (.println *out* "x")
  (require 'chic.main :reload-all)

  ;; (hui/doui (alter-var-root #'clojure.core/*warn-on-reflection* (fn [_] true)))
  (vreset! *scale2 10)
  (vreset! *scale2 2)
  (vreset! *scale2 1.75)
  (vreset! *scale2 1.5)
  (vreset! *scale2 1.25)
  (vreset! *scale2 1)
  (vreset! *scale2 nil)
#!
  )
