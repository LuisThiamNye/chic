(ns chic.fs
  (:require
   [potemkin :refer [doit]]
   [chic.debug :as debug])
  (:import
   (java.nio.file WatchService WatchKey WatchEvent WatchEvent$Kind StandardWatchEventKinds
                  Path FileSystems ClosedWatchServiceException)))

(def kw->standard-watch-evt-kind
  {:create StandardWatchEventKinds/ENTRY_CREATE
   :modify StandardWatchEventKinds/ENTRY_MODIFY
   :delete StandardWatchEventKinds/ENTRY_DELETE
   :overflow StandardWatchEventKinds/OVERFLOW})

(def ^:private -watch-service-default {:nrefs 0 :resource nil :wkey->handler {}})

(definterface IStdWatchHandler
  (overflow [^int count ctx])
  (create [^int count ^java.nio.file.Path path])
  (modify [^int count ^java.nio.file.Path path])
  (delete [^int count ^java.nio.file.Path path]))

(defn make-watch-service-loop [^WatchService watch-service]
  (fn []
    (loop []
      (when-some [^WatchKey wkey (try (.take watch-service)
                                      (catch InterruptedException _)
                                      (catch ClosedWatchServiceException _))]
        (locking #'-watch-service-default
          (when-some [handler ^IStdWatchHandler ((:wkey->handler -watch-service-default) wkey)]
           (doit [^WatchEvent evt (.pollEvents wkey)]
             (let [kind (.kind evt)
                   n (.count evt)
                   ctx (.context evt)]
               (condp identical? kind
                 StandardWatchEventKinds/OVERFLOW
                 (.overflow handler n ctx)
                 StandardWatchEventKinds/ENTRY_MODIFY
                 (.modify handler n ctx)
                 StandardWatchEventKinds/ENTRY_CREATE
                 (.create handler n ctx)
                 StandardWatchEventKinds/ENTRY_DELETE
                 (.delete handler n ctx))))))
        (when (.reset wkey)
         ;; key still valid
          (recur))))))

(defn close-watch-default [^WatchKey wkey]
  (.cancel wkey)
  (alter-var-root
   #'-watch-service-default
   (fn [{:keys [nrefs ^WatchService resource wkey->handler]}]
     {:nrefs (dec nrefs)
      :resource (if (== 1 nrefs)
                  (do (.close resource) nil)
                  resource)
      :wkey->handler (dissoc wkey->handler wkey)}))
  nil)

(deftype DirWatchKeyDefault [^WatchKey wkey]
  java.lang.AutoCloseable
  (close [_] (close-watch-default wkey)))

(defn open-watch!
  "Overflow event is always included"
  ^java.lang.AutoCloseable [^Path path kinds handler]
  (let [*ret (proteus.Containers$O. nil)]
    (alter-var-root
     #'-watch-service-default
     (fn [{:keys [nrefs resource wkey->handler]}]
       (let [resource (or resource
                          (let [r (.newWatchService (FileSystems/getDefault))]
                            (doto (Thread/ofVirtual)
                              (.name "chic.fs watch service loop")
                              (.start (make-watch-service-loop r)))
                            r))
             wkey (.register path resource (into-array WatchEvent$Kind kinds))]
         (.set *ret wkey)
         {:nrefs (inc nrefs)
          :resource resource
          :wkey->handler (assoc wkey->handler wkey handler)})))
    (->DirWatchKeyDefault (.-x *ret))))

;; alter-var-root synchronises on the var object
