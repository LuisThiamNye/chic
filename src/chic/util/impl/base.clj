(ns chic.util.impl.base
  (:require
    [clojure.tools.analyzer.jvm.utils :as ana.jvm.utils])
  (:import
    (clojure.lang Symbol Var Keyword)))

(defmacro <- [& forms] `(->> ~@(reverse forms)))

(deftype NotFound [])
(def ^:const not-found NotFound)
(defmacro not-found? [ret]
  (list `identical? ret NotFound))

(defn primitive-name->class [nam]
  (case nam
    "byte" Byte/TYPE
    "short" Short/TYPE
    "int" Integer/TYPE
    "long" Long/TYPE
    "char" Character/TYPE
    "double" Double/TYPE
    "float" Float/TYPE
    "boolean" Boolean/TYPE))

(defn tag-class ^Class [tag]
  (ana.jvm.utils/maybe-class tag)
  #_(if (or (symbol? tag) (string? tag))
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

(defn local-binding-tag ^Class [binding]
  (if (instance? clojure.lang.Compiler$LocalBinding binding)
    (let [binding ^clojure.lang.Compiler$LocalBinding binding]
      (when (.hasJavaClass binding) (.getJavaClass binding)))
    (:tag binding)))

(defmacro cond-class-isa [x & clauses]
  (let [clauses (vec clauses)
        fallback (when (odd? (count clauses)) (peek clauses))
        pairs (vec (partitionv 2 clauses))
        xrepeated? (< 1 (count pairs))
        x-sym (if xrepeated? (gensym "x_") x)
        pair-iter (.iterator ^Iterable pairs)
        body ((fn claus []
                (if (.hasNext pair-iter)
                  (let [[cls expr] (.next pair-iter)]
                    (list 'if (list '. (if (symbol? cls) 
                                         (list `identity cls)
                                         (vary-meta cls assoc :tag "Class"))
                                'isAssignableFrom x-sym)
                      expr
                      (claus)))
                  fallback)))]
    (if xrepeated? (list 'let* [x-sym x] body) body)))

(defn case-instance* [env x pairs fallback]
  (let [npairs-cond-thrs 6
        pairs (cond->> pairs (symbol? x)
                (mapv (fn [[cls expr]]
                        (let [cls (if (class? cls) cls (resolve cls))]
                          [cls `(let* [~x ~(with-meta x
                                             {:tag (.getName ^Class cls)})]
                                  ~expr)]))))]
    (cond (zero? (count pairs))
      fallback
      (<= (count pairs) npairs-cond-thrs)
      `(cond ~@(mapcat
                 (fn [[cls expr]]
                   [`(instance? ~cls ~x) expr])
                 pairs)
         true ~fallback)
      :else
      `(case (.getClass ~(cond-> x
                           (nil? (local-binding-tag (get env x)))
                           (vary-meta assoc :tag `Object)))
         ~@(into [] cat pairs)
         ~fallback))))

(defmacro case-instance [x & clauses]
  (let [fallback (when (odd? (count clauses))
                   (last clauses))
        pairs (partitionv 2 clauses)]
    (case-instance* &env x pairs fallback)))

(defn simple-symbol ^Symbol [x]
  (case-instance x
    Symbol (if (.getNamespace x)
             (Symbol/intern nil (.getName x))
             x)
    String (Symbol/intern nil x)
    Var (Symbol/intern nil (.getName (.-sym x)))
    Keyword (Symbol/intern nil (.getName (.-sym x)))
    Class (Symbol/intern nil (.getName x))
    clojure.lang.Namespace (.getName x)
    (throw (IllegalArgumentException. "No conversion to symbol"))))
