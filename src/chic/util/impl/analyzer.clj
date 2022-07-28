(ns chic.util.impl.analyzer
  (:require
    [chic.util.impl.base :as impl.base :refer [primitive-name->class tag-class]]
    [clojure.tools.analyzer.jvm :as ana]
    [clojure.tools.analyzer.passes.jvm.emit-form :as jvm.emit-form]
    [clojure.tools.analyzer.passes.jvm.infer-tag :as ana.jvm.infer-tag]
    [clojure.tools.analyzer.passes.jvm.constant-lifter :as anap.constant-lifter]
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
  (:tag ((ana.passes/schedule
           #{#'ana.jvm.infer-tag/infer-tag})
         ast)))

(defn infer-tag
  "If a local, looks up the tag in env. Else analyses the form.
   Returns nil if tag cannot be determined."
  ^Class [expr env]
  (tag-class
    (if-some [binding (get env expr)]
      (or (impl.base/local-binding-tag binding)
        (when (map? binding)
          (ana-ast-infer-tag binding)))
      (binding [ana/run-passes (ana.passes/schedule #{#'ana.jvm.infer-tag/infer-tag})]
        (:tag (ana/analyze expr (assoc (ana/empty-env) :locals env
                                  #_(update-vals env localbinding->ana-ast))))))))

(defn analyze-const? [expr env]
  (= :const
    (:op (binding [ana/run-passes
                   (ana.passes/schedule
                     #{#'anap.constant-lifter/constant-lift})]
           (ana/analyze
             expr (assoc (ana/empty-env)
                    :locals
                    (update-vals env localbinding->ana-ast)))))))

(comment
  (walk-tails #(do (prn (:form %)) %) --ast)

  (-> --ast :body :ret :thens debug/puget-prn)

  (ana/macroexpand-all '(case 4 t 'p))
  (ana/macroexpand-all '(new "nil"))
  
  )