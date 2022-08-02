(ns chic.util.impl.analyzer
  (:require
    [chic.util.impl.base :as impl.base :refer [primitive-name->class tag-class]]
    [riddley.walk :as rwalk]
    [riddley.compiler :as rcomp]
    [clojure.tools.analyzer
     [jvm :as ana]
     [env :as ana.env]
     [passes :as ana.passes]]
    [clojure.tools.analyzer.passes
     [uniquify :refer [uniquify-locals]]]
    [clojure.tools.analyzer.passes.jvm
     [emit-form :as jvm.emit-form]
     [infer-tag :as ana.jvm.infer-tag]
     [validate :as anap.validate]
     [analyze-host-expr :refer [analyze-host-expr]]
     [constant-lifter :as anap.constant-lifter]] )
  (:import
    (java.lang.reflect Field Method)
    (java.lang.invoke MethodHandles$Lookup MethodHandle MethodHandles MethodType)
    (clojure.lang Reflector)
    (clojure.lang Compiler$C Compiler$Expr Compiler$LocalBinding Compiler$ConstantExpr)))

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
         (map {"emit-form" #'jvm.emit-form/emit-form
               ; "warn-on-reflection"
               ; "warn-earmuff"
               "uniquify-locals" #'uniquify-locals
               ; "source-info"
               ; "elide-meta"
               "constant-lift" #'anap.constant-lifter/constant-lift
               ; "trim"
               ; "box"
               "analyze-host-expr" #'analyze-host-expr
               ; "validate-loop-locals"
               "validate" #'anap.validate/validate
               "infer-tag" #'ana.jvm.infer-tag/infer-tag
               ; "classify-invoke"
               })
         ks)))

(defn build-ast 
  ([form] (build-ast form (ana/empty-env) {}))
  ([form env] (build-ast form env {}))
  ([form env opts]
   (binding [ana/run-passes identity]
     (ana/analyze form env opts))))

(defn localbinding->ana-ast [obj]
  (if (map? obj)
    obj
    (let [obj ^clojure.lang.Compiler$LocalBinding obj]
      {:op :binding
       :name (.-sym obj)
       :form (.-sym obj)
       :local (if (.-isArg obj) :arg :let)
       :tag (when (.hasJavaClass obj) (.getJavaClass obj))
       ;; :init {}
       :children [] #_[:init]})))

(defn ana-ast-infer-tag ^Class [ast]
  (:tag ((ana.passes/schedule (jvm-passes ['infer-tag]))
         ast)))

(defmacro mh-invoke
  "Use in place of MethodHandle::invoke (which does not appear
to work from Clojure."
  [mh & args]
  `(.invokeWithArguments ~mh (doto (object-array ~(count args))
                               ~@(map-indexed (fn [i arg]
                                                (list `aset i arg))
                                   args))))

(let* [lk (MethodHandles/privateLookupIn Compiler$Expr (MethodHandles/lookup))]
  (let* [mh (.findVirtual lk Compiler$Expr "hasJavaClass"
              (MethodType/methodType Boolean/TYPE))]
    (defn cexpr-has-tag? ^Boolean [cexpr]
      (mh-invoke mh cexpr)))
  (let* [mh (.findVirtual lk Compiler$Expr "getJavaClass"
              (MethodType/methodType Class))]
    (defn cexpr-tag-class ^Class [cexpr]
    (mh-invoke mh cexpr))))

(comment
  (cexpr-tag-class (Compiler/analyze Compiler$C/EXPRESSION '(int 4)))
  )

#_(defn oinvoke-private* [o mname & args]
  (let [c (class o)
        ^java.lang.reflect.Method m
        (first (filter (fn [^java.lang.reflect.Method jm]
                         (and (= mname (.getName jm))
                           (= (count args) (.getParameterCount jm))))
                 (.getDeclaredMethods c)))]
    (.setAccessible m true)
    (.invoke m o (into-array Object args))))

(defn infer-tag
  "If a local, looks up the tag in env. Else analyses the form.
   Returns nil if tag cannot be determined."
  ^Class [expr env]
  (if ana.env/*env*
    (tag-class
      (if-some [binding (get env expr)]
        (ana-ast-infer-tag binding)
        (binding [ana/run-passes (ana.passes/schedule (jvm-passes ['infer-tag]))]
          (:tag (ana/analyze expr (assoc (ana/empty-env) :locals env))))))
    (let [binding ^Compiler$LocalBinding (get env expr)]
      (if (and binding (.hasJavaClass binding))
        (.getJavaClass binding)
        (let [cexpr (Compiler/analyze Compiler$C/EXPRESSION expr)]
          (when (cexpr-has-tag? cexpr)
            (cexpr-tag-class cexpr)))))))

(defn infer-form-tag ^Class [form env]
  (or (-> form meta :tag tag-class)
    (infer-tag form env)))

(defn analyze-const?
  "Returns true if expr is a compile-time constant literal"
  [expr env]
  (= :const
    (:op (binding [ana/run-passes
                   (ana.passes/schedule (jvm-passes ['constant-lift]))]
           (ana/analyze
             expr (assoc (ana/empty-env)
                    :locals
                    (update-vals env localbinding->ana-ast)))))))

#_(defn analyze-const?
    "Returns true if expr would be lifted out to a static field."
    [expr]
    (= Compiler$ConstantExpr
      (class (Compiler/analyze Compiler$C/EXPRESSION expr))))

(comment
  ;; toola.ana
  (= true (analyze-const? '[1 2 {}] {}))
  ;; Compiler/analyze
  #_#_#_#_
  (= true (analyze-const? '[1 2 {1 2}]))
  (= false (analyze-const? {}))
  (= false (analyze-const? [1 2 {}])) ;; strange
  (= false (analyze-const? []))
  
  )

(defn free-variables 
  ([form] (free-variables form (rcomp/locals)))
  ([form env]
   (let [*frees (volatile! (transient #{}))]
     (with-bindings {Compiler/LOCAL_ENV env}
       (rwalk/walk-exprs
         (fn pred [form]
           (symbol? form))
         (fn handler [form]
           (when-not (contains? (rcomp/locals) form)
             (vswap! *frees conj! form))
           form)
         form)
       (persistent! @*frees)))))

(comment
  (= #{'x} (free-variables '(do x)))
  (= #{'x} (free-variables '(do x (quote (a b)))))
  (= #{'x} (free-variables '(do x (let [x 1] x))))
  (= #{} (free-variables '(let [x 1])))
  (= #{} (free-variables '(let [x 1] x)))
  (= #{} (free-variables '(let [x 1] x a) {'a {}}))
  )

(comment
  (walk-tails #(do (prn (:form %)) %) --ast)

  (-> --ast :body :ret :thens debug/puget-prn)

  (ana/macroexpand-all '(case 4 t 'p))
  (ana/macroexpand-all '(new "nil"))
  
  )