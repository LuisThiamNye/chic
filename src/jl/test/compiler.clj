(ns jl.test.compiler
  (:require
    [jl.test.util :as test.util :refer [with-test-deps eval-str]]
    [clojure.test :refer [deftest is testing] :as test]
    [jl.compiler.analyser :as ana]
    [jl.interop :as interop]))

(declare samples)
(defn load-samples []
  (.bindRoot #'samples
    (into {}
      (map (juxt (comp :string first) second))
      (partitionv 2 (ana/str->ast (slurp "src2/test/samples.sq"))))))
(load-samples)

(defn eval-sample [n]
  (test.util/eval-ast-node
    (or ;(get samples n)
      (do (load-samples) (get samples n))
      (throw (ex-info "Sample not found" {:name n})))))

;; Tests

(deftest test-discard
  (is (= [] (ana/str->ast "#_#_(+) (-)")))
  (is (= [] (ana/str->ast "#_(+ 1 #_2)"))))

(deftest test-metadata
  (is (= 2 (count (:children(:tag(:node/meta (first (ana/str->ast "^a ^b x")))))))))

(deftest test-arithmetic
  (is (= 3 (eval-str "(+ 1 2)"))))

(deftest test-let
  (is (= 3 (eval-str "(let x 1 y 2 (+ x y))")))
  (is (nil? (eval-str "(let)")))
  (is (= 3 (eval-str "(let x 1 y (+ x 2))")))
  (testing "shadowing"
    (is (true? (eval-str "(let x false x true x)")))
    (is (true? (eval-str "(let x false (let x true x))")))))

(deftest test-imperative-local-bind
  (is (= 3 (eval-str "(do (=: x \"x\") (=: x 2) (+ x 1))")))
  (is (= 1 (eval-str "(do (=: x 1) (let x 2 nil) x)")))
  (is (= 1 (eval-str "(=: x 1)")))
  (testing "shadowing"
    (eval-str "(do (=: x false) (=: x true) x)"))
  (is (eval-str "(and (=: x true) x)"))
  ;; context - expressions and statements
  (is (eval-str "(do (< 2 (=: x 1)) true)"))
  (is (eval-str "(do (if (= 1 (=: x 1)) (set! x 2) nil) true)")))

(deftest test-and
  (is (true? (eval-str "(and true)")))
  (is (false? (eval-str "(and true false)")))
  (is (true? (eval-str "(and true true)"))))

; (= 568995840 (eval-sample "euler-8"))

;(nil? (eval-sample "hello-world"))

(deftest test-loop
  (is (= 1 (eval-str "(loop [i 1] i)")))
  (is (= 6 (eval-str "(loop [i 0] (if (<= i 5) (recur (inc i)) i))")))
  (testing "shadowing"
    (is (true? (eval-str "(let x false (loop [x true] x))"))))
  (is (true? (eval-sample "adjacent-loop-recur")))
  (is (true? (eval-sample "closest-loop-recur"))))

(eval-str "(loop [o true x false]
(if o (do (=: x false) (recur false true)) x))")
(eval-str "(loop [o true x false]
(if o (let x false (recur false true)) x))")

(deftest test-trycatch
  (is (true? (eval-sample "exception-catch")))
  (is (true? (eval-sample "exception-catch-union")))
  (is (true? (eval-sample "exception-catch-nested-rethrow")))
  (is (true? (eval-sample "exception-catch-order")))
  (is (true? (eval-sample "exception-finally")))
  (is (true? (eval-sample "exception-catch-finally"))))

(deftest test-loop-trycatch
  (is (true? (eval-sample "loop-recur-across-try")))
  (is (true? (eval-sample "loop-recur-across-catch")))
  (is (true? (try (eval-sample "loop-recur-across-finally") false
               (catch RuntimeException _ true))))
  (is (true? (eval-sample "loop-recur-within-finally")))
  (is (instance? Exception (eval-str "(nw java.lang.Exception \"hello\")"))))


(deftest test-arrays
  (is (= "[Ljava.lang.String;"
    (.getName (class (eval-str "(na java.lang.String 0)")))))
  (is (= "[S"(.getName (class (eval-str "(na short 0)")))))

  (is (true? (java.util.Arrays/equals (short-array [5])
               (eval-str "(do (=: a (na short 1)) (sa a 0 5) a)"))))
  (is (= 5 (eval-str "(do (=: a (na short 1)) (sa a 0 5) (aa a 0))"))))

#_
(let [cl (clojure.lang.RT/makeClassLoader)]
  (-> (ana/str->ast "(defclass repl.Tmp)")
    first (ana/-analyse-node)
    (->> (compiler/eval-ast cl)))
  (identical? cl (.getClassLoader (Class/forName "repl.Tmp"))))

#_
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

(deftest test-case-int
  ;; tableswitch
  (is (true? (eval-str "(case 2 1 false 2 true 3 false false)")))
  (is (true? (eval-str "(case 2 1 false 2 true 5 false false)")))
  ;; tableswitch fail
  (is (true? (eval-str "(case 0 1 false 2 false 3 false true)")))
  ;; lookupswitch
  (is (true? (eval-str "(case 2 1 false 2 true false)")))
  (is (true? (eval-str "(case 20 1 false 20 true false)")))
  ;; lookupswitch fail
  (is (true? (eval-str "(case 3 1 false 2 false true)"))))

(deftest test-reify
  (is (= Object (.getSuperclass (class (eval-str "(reify Object)")))))
  (is (= "string" (str (eval-str "(reify Object (toString [_] \"string\"))"))))
  (let [o (eval-str "(reify java.util.function.Supplier (get ^Object [_] \"x\"))")]
    (is (instance? java.util.function.Supplier o))
    (is (= "x" (str (.get o)))))

  (is (= "x" (str (.get
                 (eval-str "
(let x \"x\"
  (reify java.util.function.Supplier
    (get ^Object [_] x)))")))))
  
  (is (thrown? Exception (eval-str "
(reify Object (egg [_]))"))))

(deftest test-keyword
  (is (= "sq.lang.Keyword"
        (.getName (class (eval-str ":x"))))))

(deftest test-collection-literals
  (with-test-deps [test-keyword]
    (is (identical? io.lacuna.bifurcan.Map/EMPTY
          (eval-str "{}")))
  
    (is (= "{:x v}" (str (eval-str "{:x \"v\"}"))))
    (is (contains? #{"{:x v, :y v}" "{:y v, :x v}"}
          (str (eval-str "{:x \"v\" :y \"v\"}"))))
  
    (is (identical? io.lacuna.bifurcan.List/EMPTY
          (eval-str "[]")))
  
    (is (= "[:x, v]" (str (eval-str "[:x \"v\"]"))))
  
    (is (identical? io.lacuna.bifurcan.Set/EMPTY
          (eval-str "#{}")))
  
    (is (contains? #{"{v, :x}" "{:x, v}"} (str (eval-str "#{:x \"v\"}"))))))

(deftest test-conversions
  (testing "coercsions"
    (is (= 1.0 (eval-str "(jc Double valueOf 1)")))
    (is (= "x" (str (eval-str "(.nth [\"x\"] 0)")))))
  (testing "boxing"
    (is (some? (eval-str "[0]")))
    (is (some? (eval-str "#{\\x}")))
    (is (some? (eval-str "{1 true}")))
    (is (some? (eval-str "(nw java.lang.ref.SoftReference 5)"))))
  (testing "unboxing"
    (is (= 0 (eval-str "(jc Integer valueOf (jc Integer valueOf 0))")))
    (is (true? (eval-str "(< 1. 2)")))
    (is (= 2. (eval-str "(+ 1. 1)"))))

  (is (true? (eval-str "(= 10 \\newline)"))))

(deftest test-case-enum
  ;; tableswitch
  (is (= 2 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript :GREEK)
ARABIC 1
GREEK 2
LATIN 3
4)")))
  (testing "non-alphabetical order"
    (is (= 2 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript :GREEK)
  ARABIC 1
  LATIN 3
  GREEK 2
  4)"))))
  ;; tableswitch fail
  (is (= 4 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript HEBREW)
ARABIC 1
GREEK 2
LATIN 3
4)")))
  ;; lookupswitch
  (is (= 2 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript :GREEK)
GREEK 2
4)")))
  ;; lookupswitch fail
  (is (= 4 (eval-str "(case-enum (jf java.lang.Character$UnicodeScript HEBREW)
GREEK 2
4)")))
  ;; no match
  (true? (eval-str "(try (case-enum (jf java.lang.Character$UnicodeScript HEBREW))
(catch java.lang.Error _ true))")))

;; TODO
; (eval-str "(when (not (or false (=: x true))) x)")
; (eval-str "(when (and (=: x true)) x)")

(deftest test-vararg-spreading
  (is (= "1 2" (eval-str "(jc String format \"%s %s\" \"1\" \"2\")")))
  (is (= "1" (eval-str "(jc String format \"%s\" \"1\")")))
  (testing "empty vararg"
    (is (some? (eval-str "(jc java.nio.file.Path of \".\")")))))

(deftest test-casting
  (is (= 5 (eval-str "(ct Object 5)")))
  (is (= 5 (eval-str "(ct int (jc Integer valueOf 5))")))
  (is (= 5 (eval-str "(ct int (ct Object 5))")))
  (is (true? (eval-str "(= 5 (jc Integer valueOf 5))")))
  (testing "return type conversion"
    (is (some? (eval-str "(reify java.util.function.Supplier (get ^Object [_] 5))")))))

(deftest test-type-unification
  (is (= 2 (eval-str "(if false 1 2)")))
  (is (= 2 (eval-str "(if false (jc Integer valueOf 1) 2)")))
  (is (= 1 (eval-str "(if false (throw (nw Error)) 1)")))
  (is (true? (eval-str "(do nil true)")))
  (is (true? (eval-str "(do (if true nil nil) true)")))
  (is (true? (eval-str "(if false nil (jc Boolean valueOf true))")))
  (testing "return context + nil"
    (is (nil? (.get (eval-str "(reify java.util.function.Supplier (get ^Object [_]
       (if true nil nil)))"))))
    (is (= 5 (.get (eval-str "(reify java.util.function.Supplier (get ^Object [_]
       (if true 5 nil)))"))))
    (is (any? (eval-str "(defclass tmp.Tmp (defn f ^String [] (if true \"\" nil)))"))))
  (is (nil? (eval-str "(if true (.close (nw java.io.StringWriter))
 (.close (nw java.io.StringWriter)))")))
  (testing "void"
    (is (true?(eval-str "(if true true
     (.close (nw java.io.StringWriter)))"))))
  (testing "jump"
    (is (true? (eval-str "(loop [] (if false (recur) true))")))
    (is (= "x" (eval-str "(loop [] (if false (recur) \"x\"))")))
    (is (true? (eval-str "(and true (loop [] (if false (recur) true)))")))
    (is (true? (eval-str "(loop [] (if false (if true (recur) (recur)) true))")))
    (is (some? (eval-str "(reify java.lang.Runnable
 (run ^void [_] (loopr [i []] [] (recur) nil)))")))))

(deftest test-case-char
  (is (true? (eval-str "(case \\x \\x true \\w false)")))
  (is (true? (eval-str "(case 10 \\newline true false)")))
  (is (true? (eval-str "(case (jc Character valueOf \\x) \\x true false)"))))

(deftest test-loopr
  (is (true? (eval-str "
(loopr [x []]
  [] false true)")))
  (is (true? (eval-str "
(loopr [x [1 2]]
  [] true false)")))
  (is (true? (eval-str "
(loopr [x [1 2]]
  [] (recur) true)")))
  (is (= 2 (eval-str "
(loopr [x [1 2]]
  [i 0] (recur (inc i)) i)")))
  (is (= 3 (eval-str "
(loopr [x [1 2]]
  [i 0] (recur (+ i (ct int x))) i)")))
  (is (= 1 (eval-str "
(loopr [x [1 2]]
  [] x false)")))
  (is (true? (eval-str "
(do
(loopr [x []] []
 (recur)
 true)
true)")))
  (is (nil? (eval-str "(loopr [i [1 2]] [] (recur))"))))

(deftest test-method-selection
  (is (some? (eval-str "(io.lacuna.bifurcan.List/from {})"))))

(deftest test-arrow-macro
  (is (= 3 (eval-str "(-> 0 (+ 1) (+ 2))"))))

(comment
  (test.util/run-tests *ns*)
  
  (test/run-test test-type-unification)

  
  
  
  
  

  
  
  )

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