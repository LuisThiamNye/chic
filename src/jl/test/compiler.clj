(ns jl.test.compiler
  (:require
    [jl.compiler.core :as compiler]
    [jl.compiler.analyser :as ana]
    [jl.interop :as interop]))

(declare samples)
(defn load-samples []
  (.bindRoot #'samples
    (into {}
      (map (juxt (comp :string first) second))
      (partitionv 2 (ana/str->ast (slurp "src2/test/samples.sq"))))))
(load-samples)

(defn analyse-ast-node [ast]
  (ana/-analyse-node
    (ana/inject-default-env ast)))

(defn eval-ast-node [ast]
  (try (compiler/eval-ast (analyse-ast-node ast))
    (catch Throwable e (.printStackTrace e) :ERROR)))

(defn eval-str [s]
  (eval-ast-node (first (ana/str->ast s))))

(defn eval-sample [n]
  (eval-ast-node
    (or ;(get samples n)
      (do (load-samples) (get samples n))
      (throw (ex-info "Sample not found" {:name n})))))

(= [] (ana/str->ast "#_#_(+) (-)"))
(= [] (ana/str->ast "#_(+ 1 #_2)"))

(= 2 (count (:children(:tag(:node/meta (first (ana/str->ast "^a ^b x")))))))

(= 3 (eval-str "(+ 1 2)"))
(= 7 (eval-str "(do (l= x 4) (+ x 3))"))

(= 568995840 (eval-sample "euler-8"))

(= 1 (eval-str "(loop [i 1] i)"))
(= 6 (eval-str "(loop [i 0] (if (<= i 5) (recur (inc i)) i))"))
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
    first (analyse-ast-node)
    (->> (compiler/eval-ast {:classloader cl})))
  (let [c (interop/find-class "repl.Tmp")
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

(= "sq.lang.Keyword"
  (.getName (class (eval-str ":x"))))

(identical? io.lacuna.bifurcan.Map/EMPTY
  (eval-str "{}"))

(= "{:x v}" (str (eval-str "{:x \"v\"}")))

(identical? io.lacuna.bifurcan.List/EMPTY
  (eval-str "[]"))

(= "[:x, v]" (str (eval-str "[:x \"v\"]")))

(identical? io.lacuna.bifurcan.Set/EMPTY
  (eval-str "#{}"))

(= "{:x, v}" (str (eval-str "#{:x \"v\"}")))

;; tableswitch
(= 2 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript :GREEK)
ARABIC 1
GREEK 2
LATIN 3
4)"))

;; tableswitch fail
(= 4 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript HEBREW)
ARABIC 1
GREEK 2
LATIN 3
4)"))

;; lookupswitch
(= 2 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript :GREEK)
GREEK 2
4)"))

;; lookupswitch fail
(= 4 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript HEBREW)
GREEK 2
4)"))

;; no match
(true? (eval-str "(try (case-enum (jf java.lang.Character$UnicodeScript HEBREW))
(catch java.lang.Error _ true))"))

(= 3 (eval-str "(let x 1 y 2 (+ x y))"))
(nil? (eval-str "(let)"))
(= 3 (eval-str "(let x 1 y (+ x 2))"))

(= Object (.getSuperclass (class (eval-str "(reify Object)"))))
(= "string" (str (eval-str "(reify Object (toString [_] \"string\"))")))
(let [o (eval-str "(reify java.util.function.Supplier (get ^Object [_] :x))")]
  (and (instance? java.util.function.Supplier o)
    (= ":x" (str (.get o)))))

(= ":x" (str (.get
  (eval-str "
(let x :x
(reify java.util.function.Supplier
(get ^Object [_] x)))"))))

(= :ERROR (eval-str "
(reify Object (egg [_]))"))

(true? (eval-str "(<- (if false false) (if true true) (if false false false))"))

(true? (eval-str "(and true)"))
(false? (eval-str "(and true false)"))
(true? (eval-str "(and true true)"))

(= 3 (eval-str "(do (=: x :x) (=: x 2) (+ x 1))"))
(= 1 (eval-str "(do (=: x 1) (let x 2 nil) x)"))

(= 1 (eval-str "(=: x 1)"))
(eval-str "(and (=: x true) x)")
;; context - expressions and statements
(eval-str "(do (< 2 (=: x 1)) true)")
(eval-str "(do (if (= 1 (=: x 1)) (set! x 2) nil) true)")

(eval-str "(do (if true 1 :x) true)")

(true? (eval-str "(do (case-enum (jf java.lang.Character$UnicodeScript HEBREW)
GREEK (jc Math min 1 2)
nil) true)"))

;; coercions
(eval-str "(jc Double valueOf 1)")
; (eval-str "(jc Integer valueOf 1.)")

;; TODO
; (eval-str "(when (not (or false (=: x true))) x)")
; (eval-str "(when (and (=: x true)) x)")

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
    (ana/inject-default-env)
    (ana/-analyse-node)
    --cleanast chic.debug/puget-prn #_
    compiler/eval-ast
    ; type
    )
  
  (def --kwcache (.get (doto (second (.getDeclaredFields clojure.lang.Keyword))
                         (.setAccessible true))
                   clojure.lang.Keyword))
  (meta
    (second
      (keys
        (frequencies
          (map (fn [x] (with-meta [(count x)] {:key x}))
            (vals (group-by #(.hashCode %) (keep #(.get %) (.values --kwcache)))))))))
  
  (frequencies
    (map (fn [x] (with-meta [(count x)] {:key x}))
      (vals (group-by #(.hashCode %)
              (take 10300 (iterate (fn [_] (Object.)) (Object.)))))))
  
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


