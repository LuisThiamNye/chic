(ns chic.util
  (:refer-clojure :exclude [compile])
  (:require
    [chic.util.impl.base :as impl.base]
    [chic.util.impl.analyzer :as impl.ana]
    [chic.util.impl.loopr :as impl.loopr]
    [clojure.walk :as walk]
    [chic.util.ns :as util.ns]
    [taoensso.encore :as enc]
    [clojure.string :as str]
    [tech.droit.fset :as fset]
    [potemkin :refer [unify-gensyms doit]]
    [riddley.walk :as rwalk]
    [io.github.humbleui.paint :as huipaint]))

(util.ns/inherit-vars
  util.ns/inherit-vars
  impl.base/<-
  impl.base/primitive-name->class
  impl.base/local-binding-tag
  impl.base/tag-class
  impl.base/case-instance
  impl.base/simple-symbol
  impl.ana/infer-tag
  impl.ana/analyze-const?
  impl.loopr/loopr)

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

(defn index-of
  "Works on APersistentVector. Returns -1 if not found."
  ^long [coll x]
  (.indexOf ^clojure.lang.APersistentVector coll x))

(defn remove-index
  ([coll at] (remove-index coll at (inc at)))
  ([coll from to-exclusive]
   (into (subvec coll 0 from)
     (subvec coll to-exclusive))))

(defn nther 
  ([index] #(nth % index))
  ([index not-found] #(nth % index not-found)))

#_(defmacro binding-thread-locals [bindvec & body]
  (let* []
    ))

(defmacro clearing-thread-locals [bindvec & body]
  (let [bindings (partitionv 2 bindvec)
        _ (assert (every? symbol? (map (nther 0) bindings)))]
    `(try ~@(map (fn [[sym expr]]
                  `(.set ~sym ~expr))
             bindings)
       ~@body
       (finally ~@(map (fn [[sym _]]
                         `(.set ~sym nil))
                    bindings)))))

(defmacro with-keep-open [bindvec & body]
  (if (zero? (count bindvec))
    `(do ~@body)
    (let [[sym init & rest] bindvec]
      `(let [~sym ~init]
         (try (with-keep-open ~(vec rest)
                ~@body)
           (catch Throwable _# (. ~sym close)))))))

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

(defn get-form-tag
  "Returns class object. Fall backs to Object."
  [form]
  (tag-class (:tag (meta form) Object)))

(defn prim-or-obj-class [^Class c]
  (if (.isPrimitive c) c Object))

(defmacro case-enum [enum & clauses]
  (let [cls (or (tag-class (:tag (meta enum))) (infer-tag enum &env))
        names->ordinals
        (when cls (reduce (fn [m ^Enum e]
                            (assoc m (.name e) (.ordinal e)))
                    {} (.getEnumConstants cls)))]
    `(case (.ordinal ~(cond-> enum (nil? cls) 
                        (vary-meta assoc :tag 'Enum)))
       ~@(mapcat (fn [[variant expr]]
                   [(or (when (simple-symbol? variant)
                          (get names->ordinals (name variant)))
                      (.ordinal ^Enum (eval variant))) expr])
           (partitionv 2 clauses))
       ~@(when (odd? (count clauses))
           [(last clauses)]))))

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

(defn -specialised-lookupthunk-reify-method [userclsname usercls-sym fld-sym]
  (unify-gensyms
    `(get [thunk# mapthing##]
       (if (identical? (class mapthing##) ~usercls-sym)
         (oaccess-field
           ~(with-meta 'mapthing## {:tag userclsname})
           ~fld-sym)
         thunk#))))

(defn -generic-lookupthunk-reify-method [userclsname usercls-sym fld-syms idx-sym]
  (let [mapsym (gensym "map-thing_")
        hinted-mapsym (with-meta mapsym {:tag userclsname})]
    `(get [thunk# ~mapsym]
       (if (identical? (class ~mapsym) ~usercls-sym)
         (case ~idx-sym
           ~@(eduction
               (map-indexed 
                 (fn [i fld-sym]
                   [(int i) `(oaccess-field ~hinted-mapsym ~fld-sym)]))
               cat
               fld-syms)
           thunk#)
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
          ~(-generic-lookupthunk-reify-method userclsname 'usercls ksyms 'idx))))))

(defn +deftype-lookup-pass* [userclsname ksyms fields opts+specs]
  (let [k-sym (gensym "k")
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
        (loopr [fld fields]
          [fmap {} mta nil]
          (<- (do (.setAccessible fld true))
            (let [nam (.getName fld)
                  v (.get fld o)])
            (if (and (nil? mta) 
                  (= "__metaExt" nam))
              (recur fmap v)
              (if (java.lang.reflect.Modifier/isStatic (.getModifiers fld))
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

(prefer-method print-method IPrintableObjectFields clojure.lang.IDeref)

(definterface IFieldSettable
  (__setField [^clojure.lang.Keyword kw v])
  (__setField [^clojure.lang.Keyword kw ^byte v])
  (__setField [^clojure.lang.Keyword kw ^short v])
  (__setField [^clojure.lang.Keyword kw ^int v])
  (__setField [^clojure.lang.Keyword kw ^long v])
  (__setField [^clojure.lang.Keyword kw ^float v])
  (__setField [^clojure.lang.Keyword kw ^double v])
  (__setField [^clojure.lang.Keyword kw ^char v])
  (__setField [^clojure.lang.Keyword kw ^boolean v]))

(defn setf! [^IFieldSettable o k v]
  (.__setField o k v))

(defn alterf! [^IFieldSettable o k f & args]
  (.__setField o k (apply f (get o k) args)))

(defn +deftype-settable-pass* [settable-syms kflds fields opts+specs]
  (let [mut-fields (into #{} 
                     (filter (fn [sym]
                               (seq (fset/select-keys (meta sym) 
                                      #{:mut :vmut}))))
                     fields)
        settable-syms (cond
                        (sequential? settable-syms) settable-syms
                        (true? settable-syms) 
                        (if kflds
                          (mapv (set kflds) mut-fields)
                          (mapv enc/without-meta mut-fields))
                        :else (throw (Exception. "Invalid :settable")))
        _ (assert (every? mut-fields settable-syms))
        by-tag (group-by (comp prim-or-obj-class get-form-tag) 
                 (map mut-fields settable-syms))
        v-sym (gensym "v")
        k-sym (with-meta (gensym "k")
                {:tag "clojure.lang.Keyword"})]
    [fields 
     (into (conj opts+specs `IFieldSettable)
       (map
         (fn [[^Class tag syms]]
           `(__setField [_# ~k-sym ~(with-meta v-sym 
                                      {:tag (.getName tag)})]
              (case ~k-sym
                ~@(mapcat (fn [sym]
                            [(keyword sym) `(set! ~(enc/without-meta sym) ~v-sym)])
                        syms)))))
       by-tag)]))

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
  (let [cust-opt-keys #{:meta :keys :settable :default-print}
        [optseq cust-opts specs] (-parse-custom-opts+specs cust-opt-keys opts+specs)
        kflds (when-some [kopt (:keys cust-opts)]
                (if (vector? kopt)
                  kopt (mapv enc/without-meta fields)))
        [fields specs] (if-some [settable-syms (:settable cust-opts)]
                         (+deftype-settable-pass* settable-syms kflds fields specs)
                         [fields specs])
        fields (mapv (fn [sym] (vary-meta sym #(when %
                                                 (fset/rename-keys
                                                   % {:mut :unsynchronized-mutable
                                                      :vmut :volatile-mutable}))))
                 fields)
        specs (vec specs)
        specs (if kflds
                (+deftype-lookup-pass* nam kflds fields specs)
                specs)
        [fields specs] (if-some [opt (:meta cust-opts)]
                         (+deftype-meta-pass* opt fields specs)
                         [fields specs])
        specs (cond-> specs 
                (and (nil? (:default-print cust-opts))
                  (not-any? #{'clojure.lang.IDeref 'IDeref 'IPrintableObjectFields} specs))
                (conj `IPrintableObjectFields))]
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




