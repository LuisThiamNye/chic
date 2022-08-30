(ns jl.test.compiler
  (:require
    [jl.compiler.core :as compiler]
    [jl.compiler.analyser :as ana]))

(declare samples)
(defn load-samples []
  (.bindRoot #'samples
    (into {}
      (map (juxt (comp :string first) second))
      (partitionv 2 (ana/str->ast (slurp "src2/test/samples.sq"))))))
(load-samples)

(defn eval-str [s]
  (compiler/eval-ast
    (ana/-analyse-node
      (first (ana/str->ast s)))))

(defn eval-sample [n]
  (compiler/eval-ast
    (ana/-analyse-node
      (or ;(get samples n)
        (do (load-samples) (get samples n))
        (throw (ex-info "Sample not found" {:name n}))))))

(= 568995840 (eval-sample "euler-8"))

(true? (eval-sample "adjacent-loop-recur"))
(true? (eval-sample "closest-loop-recur"))

(true? (eval-sample "exception-catch"))
(true? (eval-sample "exception-catch-union"))
(true? (eval-sample "exception-catch-nested-rethrow"))
(true? (eval-sample "exception-catch-order"))
(true? (eval-sample "exception-finally"))
(true? (eval-sample "exception-catch-finally"))

(true? (eval-sample "loop-recur-across-try"))
(true? (eval-sample "loop-recur-across-catch"))
(try (eval-sample "loop-recur-across-finally") false
  (catch RuntimeException _ true))
(true? (eval-sample "loop-recur-within-finally"))

(nil? (eval-sample "hello-world"))

(= "[Ljava.lang.String;"
  (.getName (class (eval-str "(na java.lang.String 0)"))))
(= "[S"(.getName (class (eval-str "(na short 0)"))))

(java.util.Arrays/equals (short-array [5])
  (eval-str "(do (l= a (na short 1)) (sa a 0 5) a)"))
(= 5 (eval-str "(do (l= a (na short 1)) (sa a 0 5) (aa a 0))"))

(comment
  (defn --cleanast [ast]
    (clojure.walk/prewalk
      (fn [m]
        (if (:node/kind m)
          [(symbol (:node/kind m))
           (dissoc m :node/kind #_:node/spec :node/locals :node/env)]
          m))
      ast))

  (-> (ana/str->ast
        ; --s #_
        "
;(if (= 1 2) 0 -1)
;(ji \"a.b\" replace \\. \\-)
; (jc java.lang.String valueOf 4)
(na java.lang.String 0)

")
    first
    (ana/-analyse-node)
    --cleanast chic.debug/puget-prn #_
    compiler/eval-ast
    ; type
    )
  )


