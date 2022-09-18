(ns jl.compiler.spec
  (:require
    [jl.interop :as interop]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)))

(defn with-cast [ctor spec])

(defn untyped-num-literal? [spec]
  (= [:number] (:traits spec)))

(defn untyped-int-literal? [spec]
  (= [:integer] (:traits spec)))

(defn specialise-num-literal [typ spec]
  {:spec/kind :exact-class
   :classname (condp = typ
                :long "long"
                :int "int"
                :byte "byte"
                :float "float"
                :double "double"
                :bigint "java.lang.BigInteger"
                :bigdec "java.lang.BigDecimal")})

(defn class-meets? [iface subject]
  (isa? (Class/forName subject) (Class/forName iface)))

(defn get-exact-class [spec]
  (assert (or (nil? spec) (contains? spec :spec/kind))
    (pr-str {:keys (keys spec)}))
  (cond
    (= :exact-class (:spec/kind spec))
    (:classname spec)
    (= :exact-array (:spec/kind spec))
    (let [c (:classname spec)
          sb (StringBuilder.)]
      (dotimes [_ (:ndims spec)]
        (.append sb "["))
      (if-some [prim(type/prim-classname->type c)]
        (.append sb (.getDescriptor prim))
        (do (.append sb "L")
          (.append sb c)
          (.append sb ";")))
      (.toString sb))))

(defn get-duck-class [env spec] ;; returns a class of the intersection of behaviours
  (if (= :union (:spec/kind spec))
    (interop/intersect-classes
      (mapv (comp (partial interop/resolve-class env)
              (partial get-duck-class env)) (:specs spec)))
    (interop/resolve-class env (get-exact-class spec))))

(defn prim? [spec]
  (#{"boolean" "byte" "short" "char" "int" "long" "float" "double" "void"}
    (get-exact-class spec)))

(defn void? [spec]
  (= "void" (get-exact-class spec)))
#_#_#_
(defn biginteger-coerce [spec]
  (let [clsname (get-exact-class spec)]
    (condp = clsname
      "[B" (with-cast spec ?)
      "java.lang.String" (with-cast spec ?)
      "long" (with-cast spec ?) ;;valueOf
      (or
        (when (#{"byte" "short" "int"})
           (with-cast spec ?)) ;;valueOf(long)
        (when (class-meets? "java.lang.BigDecimal" clsname)
           (with-cast spec ?))
        (when (untyped-int-literal? spec)
          (with-cast ? (specialise-num-literal :long spec)))))))

(defn bigdecimal-coerce [spec]
  (let [clsname (get-exact-class spec)]
    (condp = clsname
      "long" (with-cast ? spec)
      "int" (with-cast ? spec)
      "double" (with-cast ? spec)
      "[C" (with-cast ? spec)
      "java.lang.String" (with-cast ? spec)
      (or (when (#{"short" "byte"} clsname)
            ;; int ctor
            (with-cast ? spec))
        (when (untyped-num-literal? spec)
          ;; string ctor
          (with-cast ? spec))))))

(defn try-coerce-class [clsname spec]
  (if (class-meets? clsname spec)
    spec
    (({"java.lang.BigInteger" biginteger-coerce
        "java.lang.BigDecimal" bigdecimal-coerce}
      clsname) spec)))

(defn trait-meets? [trait subject]
  (or (= trait subject)
    (when (= trait :number)
      (= subject :integer))))

(defn certain-trait? [trait spec]
  (when (= :traits (:spec/kind spec))
    (some (partial trait-meets? trait) (:traits spec))))

(defn try-coerce-trait [trait spec]
  (when (certain-trait? trait spec)
    spec))

(defn get-certain-trait [spec trait]
  (when (= :traits (:spec/kind spec))
    (get-in spec [:overrides trait])))

(defn link-multispec [mspec specs]
  (let [aspecs (mapv (fn [spec]
                       {:spec/kind :atom
                        :atom (atom spec)})
                 specs)]
    (mapv (fn [spec]
            spec) ;; todo
      aspecs)))

(defn get-array-element [spec]
  (if (= :exact-array (:spec/kind spec))
    (if (= 1 (:ndims spec))
      {:spec/kind :exact-class
       :classname (:classname spec)}
      (update spec :ndims dec))
    (throw (ex-info "spec not an array"
             {:spec spec}))))

(defn of-class [classname]
  (let [bks (re-find #"^\[+" classname)
        ndims (count bks)]
    (if (= 0 ndims)
      {:spec/kind :exact-class
       :classname classname}
      (let [c (subs classname ndims)]
        {:spec/kind :exact-array
         :classname (or (second (re-matches #"L(.+);" c))
                      (.getClassName (Type/getType c)))
         :ndims ndims}))))






