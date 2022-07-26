(ns chic.util
  (:refer-clojure :exclude [compile])
  (:require
    [clojure.walk :as walk]
    [taoensso.encore :as enc]
    [clojure.string :as str]
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

(defn index-of ^long 
  "Works on APersistentVector. Returns -1 if not found."
  [coll x]
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

(defmacro <- [& forms] `(->> ~@(reverse forms)))

(macroexpand-1 '(as-> 1 x (prn 1) (prn 2) (prn 3)))

(defmacro inline<- [sym & exprs]
  (loop [templates (reverse (butlast exprs))
         child (last exprs)]
    (if-some [tp (first templates)]
      (recur (next templates)
        (let [*found? (volatile! false)
              st (walk/postwalk
                   (fn [expr]
                     (if (= expr sym)
                       (do (assert (not @*found?))
                         (vreset! *found? true)
                         child)
                       expr))
                   tp)]
          (if @*found?
            st
            `(->> ~child ~st))))
      child)))

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

(defn primitive-name->class [nam]
  (.componentType ^Class
    (case nam
      "byte" (Class/forName "[B")
      "short" (Class/forName "[S")
      "int" (Class/forName "[I")
      "long" (Class/forName "[J")
      "char" (Class/forName "[C")
      "double" (Class/forName "[D")
      "float" (Class/forName "[F")
      "boolean" (Class/forName "[Z"))))

(defn normalise-tag ^Class [tag]
  (if (or (symbol? tag) (string? tag))
    (let [r (resolve (symbol tag))]
      (if (class? r) r 
        (case (str tag)
          "bytes" (Class/forName "[B")
          "shorts" (Class/forName "[S")
          "ints" (Class/forName "[I")
          "longs" (Class/forName "[J")
          "chars" (Class/forName "[C")
          "doubles" (Class/forName "[D")
          "floats" (Class/forName "[F")
          "booleans" (Class/forName "[Z")
          (primitive-name->class (str tag)))))
    tag))

(defn get-sym-tag
  "Class object or string if primitive."
  [sym]
  (normalise-tag (:tag (meta sym) Object)))

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
(defn int-range-it
  (^java.util.Iterator []
    (IntRangeIteratorInf. -1))
  (^java.util.Iterator [n]
    (IntRangeIterator. -1 (dec n)))
  (^java.util.Iterator [start end]
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

(defmacro oaccess-field [o fld]
  `(. ~o ~(symbol (str "-" fld))))

#_(def ^:dynamic *unquote-exprs* nil)

#_(defmacro compile-dynamic-unquote [n]
  (nth *unquote-exprs* n))

#_(defmacro reify-template [code]
  (let [*unquotes (volatile! [])
        code2 (rwalk/walk-exprs (fn [node] (and (seq? node) (= `unquote (first node))))
                (fn [expr]
                  (vswap! *unquotes conj (second expr))
                  `(compile-dynamic-unquote ~(dec (count @*unquotes))))
                code)]
    `(binding [*unquote-exprs* [~@(deref *unquotes)]]
       (rwalk/macroexpand-all '~code2))))

(comment #_
  (let [x '(* 2 3)]
    (reify-template (let [x 0] (+ 1 ~'x)))))

(defmacro with-gensyms [syms & body]
  `(let* [~@(mapcat (fn [sym]
                        [sym `(gensym ~(str sym "_"))]) syms)]
     ~@body))

(def ^:dynamic *macroexpand-creates-class* true)

(defmacro instantiate-compiled [& forms]
  (let [init-args (butlast forms)
        cls ^Class 
        (when *macroexpand-creates-class*
          (with-bindings {clojure.lang.Compiler/LOADER
                          (clojure.lang.RT/makeClassLoader)}
            (eval (last forms))))]
    `(new ~(symbol (if cls (.getName cls) "placeholder")) ~@init-args)))

(defn -specialised-lookupthunk-reify-method [userclsname #_usercls-sym fld-sym]
  (unify-gensyms
    `(get [thunk# mapthing##]
       ;(if (identical? (class mapthing##) ~usercls-sym)
         (oaccess-field
           ~(with-meta 'mapthing## {:tag userclsname})
           ~fld-sym)
         ;thunk#)
    )))

(defn -generic-lookupthunk-reify-method [userclsname #_usercls-sym fld-syms idx-sym]
  (let [mapsym (gensym "map-thing_")
        hinted-mapsym (with-meta mapsym {:tag userclsname})]
    `(get [thunk# ~mapsym]
       (case ~idx-sym
         ~@(eduction
             (map-indexed 
               (fn [i fld-sym]
                 [(int i) `(oaccess-field ~hinted-mapsym ~fld-sym)]))
             cat
             fld-syms)
         thunk#))))

#_(defn -specialised-lookupthunk-method []
  `(getLookupThunk [self# k#]
    (let [~'usercls (class self#)]
      (case k#
        ~@(mapcat 
            (fn [fsym]
              [(keyword fsym)
               `(reify clojure.lang.ILookupThunk
                  ~(-specialised-lookupthunk-reify-method userclsname fsym))])
            (take max-lookupthunks ksyms))
        nil))))

(defn -generic-lookupthunk-method [userclsname ksyms]
  `(getLookupThunk [self# k#]
    (let [~'usercls (class self#)]
      (when-some [~'idx ^"int" (case k#
                                 ~@(eduction 
                                     (map-indexed
                                       (fn [i fld-sym]
                                         [(keyword fld-sym) (int i)]))
                                     cat ksyms))]
        (reify clojure.lang.ILookupThunk
          ~(-generic-lookupthunk-reify-method userclsname ksyms 'idx))))))

(defn +deftype-lookup-pass* [userclsname kopt fields opts+specs]
  (let [ksyms (if (or (vector? kopt) (set? kopt))
                kopt (mapv enc/without-meta fields))
        k-sym (gensym "k")
        case-expr (fn [nf]
                    `(case ~k-sym
                       ~@(mapcat (fn [sym]
                                   [(keyword sym) sym])
                           ksyms)
                       ~nf))
        nf-sym (gensym "nf")
        ;max-lookupthunks 4
        nilnf-inline-thrs 5]
    (into opts+specs
      `[clojure.lang.ILookup clojure.lang.IKeywordLookup
        (valAt [self# ~k-sym]
          ~(if (< nilnf-inline-thrs (count ksyms))
             `(valAt self# ~k-sym nil)
             (case-expr nil)))
        (valAt [_# ~k-sym ~nf-sym]
          ~(case-expr nf-sym))
        clojure.lang.ILookupThunk
        ~(-generic-lookupthunk-method userclsname ksyms)])))

(defn +deftype-meta-pass* [opt fields opts+specs]
  (let [mut (#{:mut :vmut} opt)
        fields (conj fields (with-meta '__metaExt
                              (case mut
                                :mut {:unsynchronized-mutable true}
                                :vmut {:volatile-mutable true}
                                nil)))
        opts+specs (into opts+specs
                     `[~@(when mut
                           `[clojure.lang.IReference
                             (alterMeta [_# f# args#]
                               (set! ~'__metaExt (apply f# args#)))
                             (resetMeta [_# m#]
                               (set! ~'__metaExt m#))])
                       clojure.lang.IMeta
                       (meta [_#] ~'__metaExt)])]
    [fields opts+specs]))

(definterface IPrintableObjectFields)

(def ^:dynamic ^io.lacuna.bifurcan.Set *observed-print-objects* 
  (io.lacuna.bifurcan.Set. 
    (reify java.util.function.ToLongFunction
      (applyAsLong [_ o]
        (hash o)))
    (reify java.util.function.BiPredicate
      (test [_ a b]
        (identical? a b)))))

(defmethod print-method IPrintableObjectFields [o ^java.io.Writer w]
  (if (.contains *observed-print-objects* o)
    (do (.write w "#")
      (.write w (.getName (class o)))
      (.write w "[<repeat>]"))
    (binding [*observed-print-objects* 
              (.add *observed-print-objects* o)]
      (let [fields (.getDeclaredFields (class o))]
        (chic.util.loopr/loopr [fld fields]
          [fmap {} mta nil]
          (<- (do (.setAccessible fld true))
            (let [nam (.getName fld)
                  v (.get fld o)])
            (if (and (nil? mta) 
                  (= "__metaExt" nam))
              (recur fmap v)
              (if (str/starts-with? nam "const__")
                (recur fmap mta)
                (recur (assoc fmap (symbol nam) v) mta))))
          (let [show-meta? (and #_*print-meta* *print-readably* (some? mta))]
            (when show-meta?
              (.write w "^")
              (print-method mta w)
              (.write w " "))
            (.write w "#")
            (.write w (.getName (class o)))
            (print-method fmap w)))))))

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

(defn -parse-custom-opts+specs [cust-opt-keys opts+specs]
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
        [optseq cust-opts (vec (drop (* 2 nopts) opts+specs))]))))

(defmacro deftype+ [nam fields & opts+specs]
  (enc/have vector? fields)
  (enc/have symbol? nam)
  (let [cust-opt-keys #{:meta :keys :settable}
        [optseq cust-opts specs] (-parse-custom-opts+specs cust-opt-keys opts+specs)
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
                (+deftype-lookup-pass* nam kopt fields specs)
                specs)
        [fields specs] (if-some [opt (:meta cust-opts)]
                         (+deftype-meta-pass* opt fields specs)
                         [fields specs])
        specs (conj specs `IPrintableObjectFields)]
    `(deftype ~nam ~fields
       ~@optseq
       ~@specs)))

(defn +reify-lookup-pass* [kopt specs]
  (let [ksyms (if (or (vector? kopt) (set? kopt))
                kopt (throw (UnsupportedOperationException.)))
        k-sym (gensym "k")
        case-expr (fn [nf]
                    `(case ~k-sym
                       ~@(mapcat (fn [sym]
                                   [(keyword sym) sym])
                           ksyms)
                       ~nf))
        nf-sym (gensym "nf")]
    (into specs
      `[clojure.lang.ILookup
        (valAt [self# ~k-sym]
          (.valAt self# ~k-sym nil))
        (valAt [_# ~k-sym ~nf-sym]
          ~(case-expr nf-sym))])))

(defmacro reify+ [& opts+specs]
  (let [cust-opt-keys #{:keys}
        [optseq cust-opts specs] (-parse-custom-opts+specs cust-opt-keys opts+specs)
        specs (if-some [kopt (:keys cust-opts)]
                (+reify-lookup-pass* kopt specs)
                specs)]
    `(reify ~@optseq ~@specs)))

(defn nther 
  ([index] #(nth % index))
  ([index not-found] #(nth % index not-found)))

(defmacro minirec [& kvs]
    (let [kvs (partitionv 2 kvs)
          svs (mapv (fn [[k v]]
                       [(symbol k) v])
               kvs)
          k-syms (mapv (nther 0) svs)]
      `(let [~@(eduction cat svs)]
         (reify+ :keys ~k-syms))))

#_(defn open []) ;; for opening resources explicitly

#_(defmacro with-kwfield-hints []
    ;; hint (:access obj) for ilookup objs
    (mapv (fn [^java.lang.reflect.Field field]
            [(.getName field) (.getName (.getType field))])
      (.getDeclaredFields cls)))

(definline equals [x y]
  `(clojure.lang.Util/equals ~x ~y))

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

(deftype+ WatchableMutable [^:mut value ^:mut ^clojure.lang.IKVReduce watches]
  :keys [watches]
  IMutable
  (reset [self v]
    (let [prev value]
      (set! value v)
      (.kvreduce watches (fn [_ k f] (f k self prev v)) nil)))
  clojure.lang.IDeref
  (deref [_] value)
  clojure.lang.IRef
  (getWatches [_] watches)
  (addWatch [_ k f] (set! watches (assoc watches k f)))
  (removeWatch [_ k] (set! watches (dissoc watches k))))

(defn watchable-mutable! ^IMutable [init]
  (WatchableMutable. init {}))

;(defmacro assert+)

(defmacro with-merge-assert-data [data & body]
  `(enc/with-dynamic-assertion-data 
     (enc/merge (enc/get-dynamic-assertion-data) ~data)
     ~@body))

(defmacro set-merge-assert-data [data]
  (when *assert*
    `(set! taoensso.truss.impl/*?data* 
       (enc/merge taoensso.truss.impl/*?data* ~data))))




