(ns chic.debug.nrepl
  (:require
    [nrepl.core :as nrepl]
    [nrepl.server]
    [nrepl.middleware.session :as middleware.session]
    [nrepl.middleware :as middleware]))

(def *root-session-bindings "Var->init" (atom {}))

(def ^:dynamic *last-eval* nil)

(do
  (defn wrap-capture-eval [h]
    (fn [{:keys [op code line session ns] :as msg}]
      (when (= "eval" op)
        (try
          (swap! session
            (fn [ses]
            (-> ses
              (assoc #'*last-eval* (-> session meta ::last-eval))
              ((partial merge-with (fn [a b]
                                     (if a a b)))
               @*root-session-bindings))))
          (alter-meta! session assoc ::last-eval
            {:code code :ns (symbol ns)})
          (catch Throwable e (.printStackTrace e))))
        (h msg)))

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
