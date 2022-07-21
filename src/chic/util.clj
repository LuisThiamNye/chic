(ns chic.util
  (:require
   [taoensso.encore :as enc]
   [tech.droit.fset :as fset]
   [clojure.tools.analyzer.jvm :as ana]
   [clojure.tools.analyzer.passes :as ana.passes]
   [clojure.tools.analyzer.passes.jvm.constant-lifter :as anap.constant-lifter]
   [clojure.tools.analyzer.passes.jvm.infer-tag :as ana.jvm.infer-tag]
   [potemkin :refer [unify-gensyms doit]]
   [riddley.walk :as rwalk]))

(def ^:constant phi-1 (double (/ (- (Math/sqrt 5) 1) 2)))
(def ^:constant phi (double (/ (+ 1 (Math/sqrt 5)) 2)))

(defn url->bytes [url]
  (with-open [^java.io.InputStream input-stream (.openStream (java.net.URL. url))]
    (loop [bs []]
      (let [b (.read input-stream)]
        (if (== -1 b)
          (byte-array bs)
          (recur (conj bs b)))))))

(defn assoc-if-not= [m k v]
  (if (= (get m k) v)
    m
    (assoc m k v)))

(defmacro compile [& body]
  (if (and (== 1 (count body))
           (seq? (first body))
           (symbol? (ffirst body))
           (= #'fn (resolve (ffirst body))))
    ((eval (first body)) &env)
    (eval `(do ~@body))))

(defmacro quoted [expr]
  `(quote ~expr))

(definterface IMutable
  (reset [v]))

(deftype Mutable [^:unsynchronized-mutable value]
  chic.util.IMutable
  (reset [_ v] (set! value v))
  clojure.lang.IDeref
  (deref [_] value))

(defn mutable! [value]
  (Mutable. value))

(definline mreset! [mut v]
  `(.reset ~(vary-meta mut assoc :tag `IMutable) ~v))

(defn index-of [coll x]
  (.indexOf ^clojure.lang.APersistentVector coll x))

(defn remove-index
  ([coll at] (remove-index coll at (inc at)))
  ([coll from to-exclusive]
   (into (subvec coll 0 from)
         (subvec coll to-exclusive))))

(def *template-var->generated-var
  (atom {}))

(defn generate-var-for-template [ctor t namsp sym]
  (assert (symbol? sym))
  (assert (some? t))
  (assert (map? t))
  (let [template (ctor t)
        params (:params template)
        _ (assert (vector? params))
        code `(fn ~sym ~params
                ~@(:body template))
        vr (intern namsp sym (eval code))]
    (alter-meta! vr (fn [_] (assoc (meta sym)
                                   ::template-ctor ctor
                                   ::code code)))
    vr))

(defn regenerate-vars-for-template [tvar]
  (let [namsp (find-ns (symbol (namespace (symbol tvar))))]
    (doit [gvar (get @*template-var->generated-var tvar)]
      (when-some [ctor (::template-ctor (meta gvar))]
        (let [sym (symbol (name (symbol gvar)))]
          (try (generate-var-for-template ctor @tvar namsp sym)
               (catch Throwable e
                 (println "error happened generating" gvar "\n"
                          (str e)))))))))

(defmacro deftemplate [sym params & body]
  `(do (def ~sym
         {:sym ~sym
          :params (quote ~params)
          :body (quote [~@body])})
       ~(when (some? (get @*template-var->generated-var (resolve sym)))
          `(regenerate-vars-for-template (var ~sym)))
       (var ~sym)))

(defmacro defgenerated [sym params tsym ctor']
  (let [ctor (eval ctor')
        tvar (resolve tsym)
        template (ctor @tvar)
        gvar (generate-var-for-template ctor @tvar *ns* sym)]
    (swap! *template-var->generated-var
           update tvar (fnil conj #{}) gvar)
    (assert (= (count params) (count (:params template))))
    gvar))

(defn sub-template-args [template & specs]
  (reduce
   (fn [template [sym typ f]]
     (when-not (some #{sym} (:params template))
       (throw (AssertionError. (str "Could not find " sym " in params of template " (:sym template) \newline
                                    (:params template "(it's nil)")))))
     (case typ
       :inline-fn
       (assoc template
              :body (rwalk/walk-exprs
                     (fn [expr]
                       (and (seq? expr) (= sym (first expr))))
                     (fn [[_ & args]]
                       (apply f args))
                     (:body template))
              :params (into [] (remove #{sym}) (:params template)))))
   template
   specs))

(defn constant-literal? [thing]
  (or (keyword? thing)
      (symbol? thing)
      (number? thing)
      (string? thing)
      (instance? java.util.regex.Pattern thing)))

(defmacro let-macro-syms [bindings & body]
  (unify-gensyms
   `(let
     ~(into `[symbinds## (java.util.ArrayList.)]
            cat
            [(eduction
              (partition-all 2)
              (mapcat
               (fn [[sym v]]
                 [sym v
                  sym `(if (constant-literal? ~sym)
                         ~sym
                         (let [lsym# (gensym ~(name sym))]
                           (.add symbinds## [lsym# ~sym])
                           lsym#))]))
              bindings)
             `[code## (do ~@body)]])
      (if (< 0 (.size symbinds##))
        (list 'let (into [] cat symbinds##)
              code##)
        code##))))

(defmacro <<- [& forms] `(->> ~@(reverse forms)))

(defn ana-ast-infer-tag ^Class [ast]
  (:tag ((ana.passes/schedule
          #{#'ana.jvm.infer-tag/infer-tag})
         ast)))

(defn local-binding-tag ^Class [binding]
  (or (:tag binding)
      (if (map? binding)
        (ana-ast-infer-tag binding)
        (let [binding ^clojure.lang.Compiler$LocalBinding binding]
          (when (.hasJavaClass binding) (.getJavaClass binding))))))

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

(defn infer-tag ^Class [expr env]
  (if-some [binding (get env expr)]
    (local-binding-tag binding)
    (binding [ana/run-passes (ana.passes/schedule #{#'ana.jvm.infer-tag/infer-tag})]
      (:tag (ana/analyze expr (assoc (ana/empty-env) :locals env
                                     #_(update-vals env localbinding->ana-ast)))))))

(defn analyze-const? [expr env]
  (= :const
     (:op (binding [ana/run-passes
                    (ana.passes/schedule
                     #{#'anap.constant-lifter/constant-lift})]
            (ana/analyze
             expr (assoc (ana/empty-env)
                         :locals
                         (update-vals env localbinding->ana-ast)))))))

(defmacro doit-zip
  "Like doseq, but based on iterators. Zips through multiple iterables until one is exhausted."
  [it-exprs & body]
  (let [bindings (partitionv 2 it-exprs)
        it-bindings
        (mapv (fn [[_ coll-expr]]
                (if (= java.util.Iterator (infer-tag coll-expr &env))
                  {:it-sym (if (symbol? coll-expr) coll-expr (gensym "iterator"))
                   :it-expr coll-expr
                   :symbol? (symbol? coll-expr)}
                  {:it-sym (gensym "iterator")
                   :it-expr `(.iterator ~(with-meta (if (seq? coll-expr)
                                                      coll-expr
                                                      `(do ~coll-expr)) {:tag "Iterable"}))}))
              bindings)
        it-syms (mapv :it-sym it-bindings)]
    `(let [~@(into [] (comp (remove :symbol?)
                            (mapcat (fn [{:keys [it-sym it-expr]}]
                                      [it-sym it-expr])))
                   it-bindings)]
       (loop []
         (when (and ~@(map (fn [it-sym]
                             `(.hasNext ~it-sym))
                           it-syms))
           (let [~@(mapcat (fn [[item-expr _] it-sym]
                             [item-expr `(.next ~it-sym)])
                           bindings it-syms)]
             ~@body)
           (recur))))))

(deftype IntRangeIteratorInf [^:unsynchronized-mutable ^int i]
  java.util.Iterator
  (hasNext [_] true)
  (next [_] (set! i (unchecked-inc-int i))))
(deftype IntRangeIterator [^:unsynchronized-mutable ^int i ^int maximum]
  java.util.Iterator
  (hasNext [_] (< i maximum))
  (next [_] (set! i (unchecked-inc-int i))))
(defn ^java.util.Iterator int-range-it
  ([]
   (IntRangeIteratorInf. -1))
  ([n]
   (IntRangeIterator. -1 (dec n)))
  ([start end]
   (IntRangeIterator. (dec start) (dec end))))

(definline iter-next [^java.util.Iterator it-expr]
  (let-macro-syms [it it-expr]
    `(when (.hasNext ~it) (.next ~it))))

(defn to-iter ^java.util.Iterator [coll]
  (if (instance? Iterable coll)
    (.iterator ^Iterable coll)
    (eduction coll)))

(defmacro <=< [lb-inclusive x ub-exclusive]
  `(and (<= ~lb-inclusive ~x) (< ~x ~ub-exclusive)))

(defn expand-class-sym [sym]
  (let [s (str sym)]
    (if (re-find #"\." s)
      sym
      (symbol (str (munge (str *ns*)) "." s)))))

(defn +deftype-lookup-pass* [kopt fields opts+specs]
  (let [ksyms (if (or (vector? kopt) (set? kopt))
                kopt (mapv enc/without-meta fields))
        k-sym (gensym "k")
        case-expr (fn [nf]
                    `(case ~k-sym
                       ~@(mapcat (fn [sym]
                                   [(keyword sym) sym])
                                 ksyms)
                       ~nf))
        nf-sym (gensym "nf")]
    (into opts+specs
          `[clojure.lang.ILookup
            (valAt [self# ~k-sym]
                   ~(if (< 5 (count ksyms))
                      `(valAt self# ~k-sym nil)
                      (case-expr nil)))
            (valAt [_# ~k-sym ~nf-sym]
                   ~(case-expr nf-sym))])))

(defn +deftype-meta-pass* [opt fields opts+specs]
  (let [fields (conj fields (with-meta '__metaExt
                              (cond (= :mut opt)
                                    {:unsynchronized-mutable true}
                                    :else {:volatile-mutable true})))
        opts+specs (into opts+specs
                         `[clojure.lang.IReference
                           (alterMeta [_# f# args#]
                                      (set! ~'__metaExt (apply f# args#)))
                           (resetMeta [_# m#]
                                      (set! ~'__metaExt m#))
                           ;; clojure.lang.IMeta
                           (meta [_#] ~'__metaExt)])]
    [fields opts+specs]))

(definterface IFieldSettable
  (__setField [^clojure.lang.Keyword kw v]))

(defn setf! [^IFieldSettable o k v]
  (.__setField o k v))

(defn alterf! [^IFieldSettable o k f & args]
  (.__setField o k (apply f (get o k) args)))

(defn +deftype-settable-pass* [fields opts+specs]
  (let [mut-fields (into [] (comp
                             (filter (fn [sym]
                                       (seq (fset/select-keys (meta sym) #{:mut :vmut}))))
                             (map enc/without-meta))
                         fields)
        v-sym (gensym "v")]
    [fields (into opts+specs
                  `[IFieldSettable
                    (__setField [_# k# ~v-sym]
                                (case k#
                                  ~@(mapcat (fn [sym]
                                              [(keyword sym) `(set! ~sym ~v-sym)])
                                            mut-fields)))])]))

(defmacro deftype+ [nam fields & opts+specs]
  (let [cust-opt-keys #{:meta :keys :settable}
        [optseq cust-opts specs]
        (let [it (to-iter (eduction (partition-all 2)
                                    (take-while (fn [[k _]] (keyword? k)))
                                    opts+specs))]
          (loop [optseq []
                 cust-opts {}
                 nopts 0]
            (if-some [[k v] (iter-next it)]
              (if (contains? cust-opt-keys k)
                (recur optseq (assoc cust-opts k v) (inc nopts))
                (recur (conj optseq k v) cust-opts (inc nopts)))
              [optseq cust-opts (vec (drop (* 2 nopts) opts+specs))])))
        [fields specs] (if (:settable cust-opts)
                              (+deftype-settable-pass* fields specs)
                              [fields specs])
        fields (mapv (fn [sym] (vary-meta sym #(when %
                                                 (fset/rename-keys
                                                  % {:mut :unsynchronized-mutable
                                                     :vmut :volatile-mutable}))))
                     fields)
        specs (vec specs)
        specs (if-some [kopt (:keys cust-opts)]
                     (+deftype-lookup-pass* kopt fields specs)
                     specs)
        [fields specs] (if-some [opt (:meta cust-opts)]
                              (+deftype-meta-pass* opt fields specs)
                              [fields specs])]
    `(deftype ~nam ~fields
       ~@optseq
       ~@specs)))


#_(defn open []) ;; for opening resources explicitly

#_(defmacro with-kwfield-hints []
    ;; hint (:access obj) for ilookup objs
    (mapv (fn [^java.lang.reflect.Field field]
            [(.getName field) (.getName (.getType field))])
          (.getDeclaredFields cls)))
