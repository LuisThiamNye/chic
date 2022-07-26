(ns chic.util.analyzer
  (:require
    [chic.debug :as debug]
    [clojure.tools.analyzer.jvm :as ana]
    [clojure.tools.analyzer.passes.jvm.emit-form :as jvm.emit-form]
    [clojure.tools.analyzer.passes :as ana.passes]))

(defn walk-tails [f ast]
  (case (:op ast)
    :do (assoc ast :ret (walk-tails f (:ret ast)))
    (:let :letfn) (assoc ast :body (walk-tails f (:body ast)))
    :if (-> ast
          (assoc :then (walk-tails f (:then ast)))
          (assoc :else (walk-tails f (:else ast))))
    :case 
    (-> ast 
      (assoc :thens (mapv(fn [x]
                           (assoc x :then (walk-tails f (:then x))))
                      (:thens ast)))
      (assoc :default (walk-tails f (:default ast))))
    (f ast)))

(defn ast-env [bindings-env]
  (assoc (ana/empty-env) :locals bindings-env))

(defn jvm-passes [ks]
  (set (eduction (map name)
         (map {"emit-form" #'jvm.emit-form/emit-form})
         ks)))

(defn build-ast 
  ([form] (build-ast form (ana/empty-env) {}))
  ([form env] (build-ast form env {}))
  ([form env opts]
   (binding [ana/run-passes identity]
     (ana/analyze form env opts))))

(ana/analyze '(let [] (if 'x true 
                        (case 'x
                          1 'a
                          2 'b
                          'c))))
(def --ast 
  (binding [ana/run-passes (ana.passes/schedule #{#'identity})]
    (ana/analyze '(case 4 t 'p))))

(comment
  (walk-tails #(do (prn (:form %)) %) --ast)

  (-> --ast :body :ret :thens debug/puget-prn)

  (ana/macroexpand-all '(case 4 t 'p))
  (ana/macroexpand-all '(new "nil"))
  
  )