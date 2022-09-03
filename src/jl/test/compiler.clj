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
      (ana/inject-default-env
        (first (ana/str->ast s))))))

(defn eval-sample [n]
  (compiler/eval-ast
    (ana/-analyse-node
      (ana/inject-default-env
        (or ;(get samples n)
         (do (load-samples) (get samples n))
         (throw (ex-info "Sample not found" {:name n})))))))


(= [] (ana/str->ast "#_#_(+) (-)"))
(= [] (ana/str->ast "#_(+ 1 #_2)"))

(= 3 (eval-str "(+ 1 2)"))
(= 7 (eval-str "(do (l= x 4) (+ x 3))"))

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

(instance? Exception (eval-str "(nw java.lang.Exception \"hello\")"))

(= "[Ljava.lang.String;"
  (.getName (class (eval-str "(na java.lang.String 0)"))))
(= "[S"(.getName (class (eval-str "(na short 0)"))))

(java.util.Arrays/equals (short-array [5])
  (eval-str "(do (l= a (na short 1)) (sa a 0 5) a)"))
(= 5 (eval-str "(do (l= a (na short 1)) (sa a 0 5) (aa a 0))"))

(let [cl (clojure.lang.RT/makeClassLoader)]
  (-> (ana/str->ast "(defclass repl.Tmp)")
    first (ana/-analyse-node)
    (->> (compiler/eval-ast cl)))
  (identical? cl (.getClassLoader (Class/forName "repl.Tmp"))))

(let [cl (clojure.lang.RT/makeClassLoader)]
  (-> (ana/str->ast "
(defclass repl.Tmp
:interfaces java.lang.Runnable java.lang.AutoCloseable
[^int x ^java.lang.String s]
(defi run [self] self x))")
    first (ana/-analyse-node)
    (->> (compiler/eval-ast cl)))
  (let [c (Class/forName "repl.Tmp")
        r (clojure.reflect/reflect c)
        fs (group-by :name (:members r))
        obj (.newInstance (first (.getDeclaredConstructors c))
              (object-array [(int 1) "x"]))
        default-field-flags #{:private :final}]
    (assert (identical? cl (.getClassLoader c)))
    (and
      (= '#{java.lang.Object java.lang.Runnable java.lang.AutoCloseable}
        (:bases r))
      (= '#{:public} (:flags r))
      (let [{:keys [type flags]} (first (get fs 's))]
        (and (= 'java.lang.String type)
          (= default-field-flags flags)))
      (let [{:keys [type flags]} (first (get fs 'x))]
        (and (= 'int type)
          (= default-field-flags flags)))
      (= '[int java.lang.String] ;; auto positional ctor
        (:parameter-types (first (get fs 'repl.Tmp))))
      (= 1 (.run obj)))))



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


")
    first
    (ana/-analyse-node)
    --cleanast chic.debug/puget-prn #_
    compiler/eval-ast
    ; type
    )
  
  (count (.get (doto (second (.getDeclaredFields clojure.lang.Keyword))
                 (.setAccessible true))
           clojure.lang.Keyword))
  
  (let [c (java.lang.ref.Cleaner/create)
        o (Object.)]
    (.register c o (fn [] (prn "clean1")))
    (.register c o (fn [] (prn "clean2")))
    nil)
  (def --cleaner (java.lang.ref.Cleaner/create))
  (def --rq (java.lang.ref.ReferenceQueue.))
  (def --ct (-> (Thread/ofVirtual)
              (.start (fn []
                        (loop []
                          (let [o(.remove --rq)]
                            (prn "removing " o)
                            (prn "also found"
                              (take-while some?
                                (iterate (fn [_] (.poll --rq)) 0)))))))))
  (def --refs
    (take 10 (iterate (fn [_] (java.lang.ref.WeakReference. (Object.) --rq)) nil)))
  ;; possible to batch process with thread loop
  
  (System/gc)
  
  ;; TODO shadowing
  
  (let [o (Object.)]
    [(.hashCode o) (.hashCode o)])
  
;; TODO something like zig's anytype
  
  (do (deftype Tmp []) nil)
  (class? (Class/forName "jl.test.compiler.Tmp"))
  
  (let [m Long/MIN_VALUE
        m' Long/MAX_VALUE
        a 3
        b 2
        x (bit-xor a b)   
        r1 (let [xn (- x)
                 y (bit-or x xn)
                 z (bit-xor m y)]
             (bit-and m z))]
   r1
    )
  (= Long/MIN_VALUE (unchecked-subtract 0 Long/MIN_VALUE))
  
  ;; TODO use Constable interface for constant folding & propagation using ConstantDynamic
  ;; TODO use ClassValue for dynamic method dispatch
  )


