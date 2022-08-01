(ns dev 
  (:require
    [riddley.walk :as rwalk]
    [chic.ui.ui3 :as ui3]
    [chic.debug.nrepl :refer [*last-eval*]]
    [user]
    [chic.debug :as debug :refer [*last-error*]]))


(do
  (def last-eval (let [le *last-eval*]
                 {:code (read-string (:code le))
                  :ns (find-ns (:ns le))}))
  (def last-code (:code last-eval)))

(debug/puget-prn
  last-code)

(binding [*ns* (:ns last-eval)
          *print-meta* true]
  (debug/puget-prn
    (rwalk/macroexpand-all last-code)))


(-> (:method-ana *last-error*)
  :body :body)

(-> *last-error*
  :method-ana
  :body :ret :body :ret)

(set! *print-length* 1000)
(set! *print-level* 7)

