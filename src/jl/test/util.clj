(ns jl.test.util
  (:require
    [jl.kickstart :as kickstart]
    [jl.compiler.analyser :as ana]
    [jl.compiler.core :as compiler]
    [clojure.test :refer [deftest is testing] :as test]))

(defn test-var [vr]
  (let [_ (test/test-var vr)
        suc? (test/successful? (test/run-test-var vr))]
    (alter-meta! vr assoc :test-success suc?)))

(defn run-tests [& nss]
  (binding [test/*report-counters* (ref test/*initial-report-counters*)]
    (doseq [ns nss]
      (test/do-report {:type :begin-test-ns
                       :ns   ns})
      (run! test-var (filter (comp :test meta) (vals (ns-interns ns))))
      (test/do-report {:type :end-test-ns
                       :ns   ns}))))

(defn test-vars-succeeded? [vars]
  (every? (comp :test-success meta) vars))

(defmacro with-test-deps [dvec & body]
  `(when (test-vars-succeeded?
           ~(mapv (fn [sym] (list 'var sym)) dvec))
     ~@body))


(defn analyse-ast-node [ast]
  (ana/-analyse-node
    (ana/inject-default-env ast)))

(defn eval-ast-node [ast]
  (kickstart/with-thread-classloader (kickstart/new-dcl)
    (compiler/eval-ast {:classloader (kickstart/new-dcl)}
      (kickstart/analyse-node {} ast))))

(defn eval-str [s]
  (eval-ast-node (first (ana/str->ast s))))
