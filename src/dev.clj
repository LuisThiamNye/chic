(ns dev 
  (:require
    [riddley.walk :as rwalk]
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

(binding [*ns* (:ns last-eval)]
  (debug/puget-prn
    (rwalk/macroexpand-all last-code)))


(-> (:ctx *last-error*)
  :raw-body-ana
  :body :op)