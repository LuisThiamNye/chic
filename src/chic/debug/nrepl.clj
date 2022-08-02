(ns chic.debug.nrepl
  (:require
    [nrepl.core :as nrepl]
    [nrepl.server]
    [nrepl.middleware.session :as middleware.session]
    [nrepl.middleware :as middleware]
    [rewrite-clj.parser :as rw.parse]))

(def *root-session-bindings "Var->init" (atom {}))

(def ^:dynamic *last-eval* nil)

(defn listen-eval-msg [{:keys [line session]}]
  ;(prn (.getContextClassLoader (Thread/currentThread)))
  #_(prn line))

(do
  (defn wrap-capture-eval [h]
    (fn [{:keys [op code line session ns] :as msg}]
      (if (= "eval" op)
        (do 
          (try
                  (listen-eval-msg msg)
                  (swap! session
                    (fn [ses]
                      (-> ses
                        (assoc #'*last-eval* (-> session meta ::last-eval))
                        ((partial merge-with (fn [a b]
                                               (if a a b)))
                         @*root-session-bindings))))
                  (alter-meta! session assoc ::last-eval
                    {:code code :ns (symbol ns)})
                  (catch Throwable e (.printStackTrace e)))
          (h (assoc msg :line (when line (inc line)))))
        (h msg))))

  (middleware/set-descriptor! #'wrap-capture-eval
    {:requires #{#'middleware.session/session}
     :expects #{"eval"}
     :handles {}}))

(defn stub-middleware! []
  (let [stub (fn [h] (partial h))
        vars [#'wrap-capture-eval]]
    (run! #(alter-var-root % (constantly stub))
      vars)))

(defn add-middleware [client]
  (nrepl/message client 
   {:op "add-middleware"
    :middleware (mapv pr-str [#'wrap-capture-eval])} ))

(defn get-middleware [client]
  (:middleware
    (first (nrepl/message client
             {:op "ls-middleware"}))))

(defn reset-middleware [client middleware]
  (nrepl/message client
    {:op "swap-middleware"
     :middleware (mapv pr-str middleware)}))

(defn refresh-middleware [client]
  (reset-middleware client
    (get-middleware client)))

(defn nrepl-session-threads []
  (eduction
    (filter #(re-find #"^nREPL-session" (.getName ^Thread %)))
    (.keySet (Thread/getAllStackTraces))))

(defn force-kill-nrepls! []
  (run! #(.stop ^Thread %) (nrepl-session-threads)))



;; Restore classloader after each eval (so they don't keep stacking up)
;; because nREPL calls clojure.main/repl each time, which appends a
;; new classloader

(require '[rewrite-clj.zip :as rw.zip])
(require '[rewrite-clj.parser :as rw.parse])

(let [ns (find-ns 'nrepl.middleware.interruptible-eval)
      vr (get (ns-map ns) 'evaluate)
      {:keys [line column file] :as mta}
      (meta vr)
      sw (java.io.StringWriter.)
      strm (.getResourceAsStream (clojure.lang.RT/baseLoader)
             "nrepl/middleware/interruptible_eval.clj")]
  (with-open [rdr (java.io.InputStreamReader. strm)]
    (if-not (and column (pos? column))
      (throw (Exception. "No column meta"))
      (loop [l 1]
        (if (= l line)
          (loop [o 1]
            (when-not (= o column)
              (.read rdr)
              (recur (inc o))))
          (if (= (.read rdr) 10)
            (recur (inc l))
            (recur l)))))
    (.transferTo rdr sw))
  (-> (rw.parse/parse-string (.toString sw))
    (rw.zip/of-node)
    (->> (iterate (comp rw.zip/rightmost rw.zip/down)))
    (nth 5)
    rw.zip/down
    rw.zip/right
    (as-> zloc
      (rw.zip/replace* zloc
        (rw.zip/node (rw.zip/rightmost (rw.zip/down zloc)))))
    rw.zip/root-string
    (as-> s (binding [*ns* ns]
              (load-string s)
              (alter-meta! vr merge mta)))))

