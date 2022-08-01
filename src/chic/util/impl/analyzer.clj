(ns chic.util.impl.analyzer
  (:require
    [chic.util.impl.base :as impl.base :refer [primitive-name->class tag-class]]
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
    (clojure.lang Reflector)
    (clojure.lang Compiler$C Compiler$Expr Compiler$LocalBinding)))

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

(def ^Method expr-hasJavaClass
  (doto ^Method (first (Reflector/getMethods Compiler$Expr 0 "hasJavaClass" false))
    (.setAccessible true)))
(def ^Method expr-getJavaClass
  (doto ^Method (first (Reflector/getMethods Compiler$Expr 0 "getJavaClass" false))
    (.setAccessible true)))

(defn oinvoke-private* [o mname & args]
  (let [c (class o)
        ^java.lang.reflect.Method m
        (first (filter (fn [^java.lang.reflect.Method jm]
                         (and (= mname (.getName jm))
                           (= (count args) (.getParameterCount jm))))
                 (.getDeclaredMethods c)))]
    (.setAccessible m true)
    (.invoke m o (into-array Object args))))

(defn cexpr-has-tag? ^Boolean [cexpr]
  (.invoke expr-hasJavaClass cexpr (object-array 0)))

(defn cexpr-tag-class ^Class [cexpr]
  (.invoke expr-getJavaClass cexpr (object-array 0)))

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

(defn analyze-const? [expr env]
  (= :const
    (:op (binding [ana/run-passes
                   (ana.passes/schedule (jvm-passes ['constant-lift]))]
           (ana/analyze
             expr (assoc (ana/empty-env)
                    :locals
                    (update-vals env localbinding->ana-ast)))))))

(comment
  (walk-tails #(do (prn (:form %)) %) --ast)

  (-> --ast :body :ret :thens debug/puget-prn)

  (ana/macroexpand-all '(case 4 t 'p))
  (ana/macroexpand-all '(new "nil"))
  
  (fn [^java.lang.reflect.Field x]
    (let [m (.getModifiers x)
          n nil]
      (chic.util/compile
        (fn [env]
          (prn (infer-tag '(java.lang.reflect.Modifier/isPrivate m) env))
))))
  
  clojure.tools.analyzer.env/*env*
  
  
  )