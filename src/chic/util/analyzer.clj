(ns chic.util.analyzer
  (:require
    [chic.debug :as debug]
   [clojure.tools.analyzer.jvm :as ana]
   [clojure.tools.analyzer.passes :as ana.passes]))

(defn walk-tails [f ast]
  (case (:op ast)
    :do (walk-tails f (:ret ast))
    (:let :letfn) (walk-tails f (:body ast))
    :if (do (walk-tails f (:then ast))
          (walk-tails f (:else ast)))
    :case (do (run! (partial walk-tails f) (eduction (map :then) (:thens ast)))
            (walk-tails f (:default ast)))
    (f ast))
  nil)

(defn ast-env [bindings-env]
  (assoc (ana/empty-env) :locals bindings-env))

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

(walk-tails #(do (prn (:form %)) %) --ast)

(-> --ast :body :ret :thens debug/puget-prn)

(ana/macroexpand-all '(case 4 t 'p))