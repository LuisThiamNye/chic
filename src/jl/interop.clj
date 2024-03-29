(ns jl.interop
  (:require
    [chic.util :as util :refer [loop-zip]]
    [jl.compiler.op :as op]
    [jl.compiler.type :as type]
    [jl.interop :as interop])
  (:import
    (org.objectweb.asm Type Opcodes)
    (java.lang.reflect Method Field Modifier Constructor Executable)))

"Accessible https://docs.oracle.com/javase/specs/jls/se18/html/jls-6.html#jls-6.6
 and applicable https://docs.oracle.com/javase/specs/jls/se18/html/jls-15.html#jls-15.12.1"

(defn find-class [name]
  ;; Ensure class initialises via its own classloader
  ;; Otherwise, if using callers' CL, it won't see updated classes
  ;; of the same name
  (let [c (Class/forName name false (clojure.lang.RT/baseLoader))]
    (try (Class/forName name true (.getClassLoader c))
      (catch VerifyError _))))

(defprotocol PNamed
  (-getName [_]))

(defprotocol PClassMember
  (-getModifiers [_]))

(defprotocol PTyped
  (-getType [_]))

(defprotocol PClassNode
  (-primitive? [_])
  (-arrayType [_])
  (-arrayClass? [_])
  (-elementType [_])
  (-hiddenClass? [_])
  (-interface? [_])
  (-satisfies? [_ super])
  (-getDeclaredMethods [_])
  (-getDeclaredFields [_])
  (-getDeclaredConstructors [_])
  (-getInterfaces [_])
  (-getSuperclass [_]))

(defprotocol PMethodNode
  (-varargs? [_])
  (-getParamTypes [_])
  (-getRetType [_]))

(extend-type Class
  PNamed
  (-getName [self] (.getName self))
  PClassMember
  (-getModifiers [self] (.getModifiers self))
  PClassNode
  (-primitive? [self] (.isPrimitive self))
  (-arrayClass? [self] (.isArray self))
  (-arrayType [self] (.arrayType self))
  (-elementType [self] (.componentType self))
  (-hiddenClass? [self] (.isHidden self))
  (-interface? [self] (.isInterface self))
  (-getInterfaces [self]
    (.getInterfaces self))
  (-getDeclaredMethods [self]
    (.getDeclaredMethods self))
  (-getDeclaredFields [self]
    (.getDeclaredFields self))
  (-getDeclaredConstructors [self]
    (.getDeclaredConstructors self))
  (-getSuperclass [self]
    (.getSuperclass self))
  (-satisfies? [self sup]
    ;; Class can only satisfy classes that already exist
    (if (class? sup)
      (.isAssignableFrom ^Class sup self)
      (= (.getName self) (-getName sup)))))

(extend-type Constructor
  PNamed
  (-getName [self] (.getName self))
  PClassMember
  (-getModifiers [self] (.getModifiers self))
  PMethodNode
  (-varargs? [self] (.isVarArgs self))
  (-getParamTypes [self] (.getParameterTypes self)))

(extend-type Method
  PNamed
  (-getName [self] (.getName self))
  PClassMember
  (-getModifiers [self] (.getModifiers self))
  PMethodNode
  (-varargs? [self] (.isVarArgs self))
  (-getParamTypes [self] (.getParameterTypes self))
  (-getRetType [self] (.getReturnType self)))

(extend-type Field
  PNamed
  (-getName [self] (.getName self))
  PClassMember
  (-getModifiers [self] (.getModifiers self))
  PTyped
  (-getType [self] (.getType self)))

(defrecord UnrealClass [name super interfaces ^int flags
                        fields methods]
  Object
  (toString [self] (str "UnrealClass:" (pr-str (:name self)))))
(extend-type UnrealClass
  PNamed
  (-getName [self] (:name self))
  PClassNode
  (-primitive? [_] false)
  (-arrayClass? [self] (:array? self))
  (-elementType [self]
    (:element-class self))
  (-arrayType [self]
    (if (= 0 (:ndims self 0))
      (assoc (->UnrealClass (str "[L" (:name self) ";")
               ;; FIXME do proper flag conversion
               Object nil (:flags self) nil nil)
        :array? true
        :ndims 1
        :element-class self
        :hidden? (:hidden? self))
      (assoc self :ndims (inc (:ndims self))
        :name (str "[" (:name self)))))
  (-hiddenClass? [self] (:hidden? self))
  (-interface? [self] (< 0 (bit-and (:flags self) Opcodes/ACC_INTERFACE)))
  (-getInterfaces [self] (:interfaces self))
  (-getDeclaredMethods [self] (or (:methods self) (object-array 0)))
  (-getDeclaredFields [self] (or (:fields self) (object-array 0)))
  (-getDeclaredConstructors [self]
    (:constructors self))
  (-getSuperclass [self] (:super self))
  (-satisfies? [self other]
    ;; could also implement by keeping set of visited bases and only checking the unvisited
    (or (identical? self other)
      (= (:name self) (-getName other))
      (-satisfies? (:super self) other)
      (some #(-satisfies? % other)
        (:interfaces self)))))

(defrecord UnrealField [name type ^int flags])
(extend-type UnrealField
  PNamed
  (-getName [self] (:name self))
  PClassMember
  (-getModifiers [self] (:flags self))
  PTyped
  (-getType [self] (:type self)))

(defrecord UnrealMethod [name param-types ret-type ^int flags])
(extend-type UnrealMethod
  PNamed
  (-getName [self] (:name self))
  PClassMember
  (-getModifiers [self] (:flags self))
  PMethodNode
  (-getParamTypes [self] (:param-types self))
  (-getRetType [self] (:ret-type self))
  (-varargs? [self] (op/acc-mask-contains? (:flags self) :varargs)))

(deftype NilClass [])
(extend-type NilClass
  PNamed
  (-getName [self] "aNilClass")
  PClassNode
  (-primitive? [_] false)
  ; (-arrayClass? [self] false)
  ; (-elementType [self])
  ; (-arrayType [self])
  (-hiddenClass? [self] false)
  (-interface? [self] false)
  ; (-getInterfaces [self] nil)
  ; (-getDeclaredMethods [self] )
  ; (-getDeclaredFields [self] )
  ; (-getDeclaredConstructors [self] )
  ; (-getSuperclass [self] Object)
  (-satisfies? [self other]
    (or (identical? self other)
      (not (-primitive? other)))))
(def nil-class (new NilClass))
(defn nil-class? [x] (identical? nil-class x))

(deftype JumpClass [])
(def jump-class (new JumpClass))
(defn jump-class? [x] (identical? jump-class x))

(defn same-class? [c1 c2]
  (or (identical? c1 c2)
    (= (-getName c1) (-getName c2))))

#_(defn map->unreal-method [m]
  ; (assert (some? (:ret-type m)))
  (map->UnrealMethod m))

(defn new-unreal-class
  ([name super interfaces]
   (->UnrealClass name super (object-array interfaces) 0 nil nil))
  ([name super interfaces opts]
   (into (->UnrealClass name super (object-array interfaces) 0
           (object-array (mapv map->UnrealField (:fields opts)))
           (object-array (mapv map->UnrealMethod (:methods opts))))
     (update (dissoc opts :fields :methods)
       :constructors #(object-array (mapv map->UnrealMethod %))))))

(defn method->type [method]
  (if (instance? Method method)
    (Type/getType ^Method method)
    (Type/getMethodType
      (type/classname->type (-getName (-getRetType method)))
      (into-array Type
        (mapv (comp type/classname->type -getName)
          (-getParamTypes method))))))

(defn ctor->type [method]
  (if (instance? Constructor method)
    (Type/getType ^Constructor method)
    (Type/getMethodType
      Type/VOID_TYPE
      (into-array Type
        (mapv (comp type/classname->type -getName)
          (-getParamTypes method))))))

(defn array-class-ndims [cls]
  (loop [i 1
         c cls]
    (let [el (-elementType c)]
      (if (-arrayClass? el)
        (recur (inc i) el)
        i))))

(defn resolve-obj-class [{:keys [new-classes class-resolver] :as env} classname]
  (or
    (get-in new-classes [classname :class])
    (when (= classname (:self-classname env))
      (:class (:self-classinfo env))) ;; only used for _.(Eval)
    (try (util/primitive-name->class classname)
      (catch Exception _))
    (when (= "void" classname) Void/TYPE)
    (if-some [info (get new-classes classname)]
      (or (:class info) (throw (ex-info "classinfo has no :class"
                                 {:classname classname})))
      (try (or (when class-resolver (class-resolver classname))
             (find-class classname))
        (catch ClassNotFoundException e
          (throw (ex-info (str "No class found: " classname)
                   {:new-classes (keys new-classes)
                    :env (select-keys env [:self-classname])
                    :env-keys (keys env)}
                   e)))
        (catch ExceptionInInitializerError e
          (throw (ClassNotFoundException. e)))))))

(defn dynamic-class? [env cls]
  (-hiddenClass? cls))

(defn primitive-name->desc [name]
  (case name
    "boolean" "Z"
    "byte" "B"
    "char" "C"
    "short" "S"
    "int" "I"
    "long" "J"
    "float" "F"
    "double" "D"
    "void" "V"))

(defn primitive-desc->name [desc]
  (case desc
    "Z" "boolean"
    "B" "byte"
    "C" "char"
    "S" "short"
    "I" "int"
    "J" "long"
    "F" "float"
    "D" "double"
    "V" "void"))

(defn split-classname [classname]
  (let [ndims (count (re-find #"^\[+" classname))
        raw (subs classname ndims)
        el-classname (if (< 0 ndims)
                       (or (second (re-matches #"L(.+);" raw))
                         (primitive-desc->name raw))
                       classname)]
    [ndims el-classname]))

(comment
  (split-classname "int")
  (split-classname "[I")
  (split-classname "[Ljava.lang.String;")
  )

(defn array-class-of [el-class ndims]
  (loop [i 0
         c el-class]
    (if (< i ndims)
      (recur (inc i) (-arrayType c))
      c)))

(defn resolve-class [env classname]
  {:post [(some? %)]}
  (let [[ndims el-classname] (split-classname classname)
        el-class (resolve-obj-class env el-classname)]
    (if (= 0 ndims)
      el-class
      (array-class-of el-class ndims))))

(defn make-unbox-conversion [box-name prim-name]
  (let [box-iname (str "java/lang/" box-name)]
    [:method :instance box-iname (str prim-name "Value")
     (str "()" (primitive-name->desc prim-name))]))

(defn make-box-conversion [box-name prim-name]
  (let [box-iname (str "java/lang/" box-name)]
    [:method :class box-iname "valueOf"
     (str "(" (primitive-name->desc prim-name) ")L" box-iname ";" )]))

(def void->nil-conversion [:insn :aconst-null])

(defn member-accessible? [c1 target mods]
  (or (Modifier/isPublic mods)
    (if (and (class? c1) (class? target))
      (let [pack2 (.getPackage target)
            pack1 (.getPackage c1)]
        ;; Note: packages are specific to a classloader.
        ;; this is too generous
        (or true ;(= (.getName pack2) (.getName pack1))
          (when (Modifier/isProtected mods)
            (.isAssignableFrom target c1))))
      ;; FIXME
      true)))

(defn method-applicable? [arg-cs desired-ret-c method method-ret-c]
  (when (or (nil? desired-ret-c)
         (-satisfies? method-ret-c desired-ret-c))
    (let [pts (-getParamTypes method)]
      (or (and (= (count pts) (count arg-cs))
            (every? identity
              (map #(-satisfies? %1 %2)
                arg-cs pts)))
          (when (and (-varargs? method) (<= (count pts) (count arg-cs)))
            (let [vararg-type (-elementType (last pts))]
              (and (every? identity
                     (map #(-satisfies? %1 %2)
                       arg-cs (butlast pts)))
                (every? #(-satisfies? % vararg-type)
                  (subvec arg-cs (dec (count pts)))))))))))

(defn sig-specificity>= [sig1 sig2]
  (every? (fn [[t1 t2]]
            (-satisfies? t1 t2))
    (map vector sig1 sig2)))

(defn most-specific-method [methods]
  (reduce (fn [ms method]
            (let [sig1 (-getParamTypes ms)
                  sig2 (-getParamTypes method)]
              (cond
                (sig-specificity>= sig1 sig2) ms
                (sig-specificity>= sig2 sig1) method
                :else (reduced nil))))
    (first methods) (next methods)))

(defn get-all-instance-methods [cls]
  (reify clojure.lang.IReduceInit
    (reduce [_ rf acc]
      (let
        [seen (java.util.HashSet.)
         rim (fn [acc cls]
               (let [ms (-getDeclaredMethods cls)]
                 (loop [acc acc
                        i (dec (alength ms))]
                   (if (<= 0 i)
                     (let [iface (aget ms i)]
                       (if (.contains seen iface)
                         (recur acc (dec i))
                         (let [acc (rf acc iface)]
                           (if (reduced? acc)
                             acc
                             (recur acc (dec i))))))
                     acc))))
         rii (fn [ri acc cls]
               (let [ifaces (-getInterfaces cls)
                     nifaces (alength ifaces)]
                 (loop [acc acc
                        j 0]
                   (if (< j nifaces)
                     (let [acc (ri acc (aget ifaces j))]
                       (if (reduced? acc)
                         acc
                         (recur acc (inc j))))
                     acc))))
         ri (fn ri [acc cls]
              (let [acc (rim acc cls)]
                (if (reduced? acc)
                  acc
                  (rii ri acc cls))))
         rc (fn rc [acc cls]
              (let [ms (-getDeclaredMethods cls)
                    acc (loop [acc acc
                               i (dec (alength ms))]
                          (if (<= 0 i)
                            (let [acc (rf acc (aget ms i))]
                              (if (reduced? acc)
                                acc
                                (recur acc (dec i))))
                            acc))]
                (if (reduced? acc)
                  acc
                  (let [acc (rii rc acc cls)]
                    (if (reduced? acc)
                      acc
                      (if-some [s (-getSuperclass cls)]
                        (recur acc s)
                        acc))))))
         acc (if (-interface? cls)
               (ri acc cls)
               (rc acc cls))]
        (if (reduced? acc) @acc acc)))))

(defn name-matching-method? [static? method-name m]
  (and (= (-getName m) method-name)
    (let [mods (-getModifiers m)]
      (and
        (= static? (Modifier/isStatic mods))
        (= 0 (bit-and Opcodes/ACC_SYNTHETIC
               #_(bit-or Opcodes/ACC_SYNTHETIC
                 Opcodes/ACC_ABSTRACT)
               mods))))))

(defn arity-matching-method? [nargs method]
  (let [nparams (count (-getParamTypes method))]
    (or (= nparams nargs)
      (let [varargs? (-varargs? method)]
        (when varargs?
          (<= (dec nparams) nargs))))))

(defn match-method-by-arg-types [methods0 arg-types]
  (let []
    (loop-zip [method methods0] ;; first find exact match
      [first-noneqs []]
      (let [param-types (-getParamTypes method)
            first-noneq
            (loop-zip [param-type ^Iterable (vec param-types)
                       i :idx]
              []
              (let [arg-type (nth arg-types i)]
                (if (same-class? arg-type param-type)
                  (recur)
                  i))
              -1)]
        (if (<= 0 first-noneq)
          (recur (conj first-noneqs first-noneq))
          method))
      ;; then try inheritance match
      (loop-zip [method ^Iterable methods0
                 first-noneq ^Iterable first-noneqs]
        [first-nomatches []
         some-match? false]
        (let [param-types (-getParamTypes method)
              first-nomatch
              (loop [i first-noneq]
                (if (< i (count param-types))
                  (let [param-type (nth param-types i)
                        arg-type (nth arg-types i)
                        matches? (-satisfies? arg-type param-type)]
                    (if matches?
                      (recur (inc i))
                      i))
                  -1))]
          (recur (conj first-nomatches first-nomatch)
            (or some-match? (< first-nomatch 0))))
        (if some-match?
          ;; find most specific of inheritance-matching methods
          (loop-zip [fnm ^Iterable first-nomatches]
            [i 0 most-specific nil]
            (if (< 0 fnm)
              (recur (inc i) most-specific)
              (let [m (nth methods0 i)
                    r (if (nil? most-specific)
                        m
                        (let [ms-params (-getParamTypes most-specific)
                              m-params (-getParamTypes m)]
                          (loop 
                            [i (dec (count m-params)) s1 false s2 false
                             diff? false]
                            (if (<= 0 i)
                              (let [p1 (nth ms-params i)
                                    p2 (nth m-params i)]
                                (recur (dec i)
                                  (or s1 (-satisfies? p2 p1))
                                  (or s2 (-satisfies? p1 p2))
                                  (or diff? (not (same-class? p1 p2)))))
                              (if diff?
                                (if (and s1 s2)
                                  (throw (ex-info "method ambiguous (no casting)"
                                           {:arg-types arg-types
                                            :method-param-types
                                            (mapv (comp vec -getParamTypes) methods0)}))
                                  (if s1 most-specific m))
                                ;; may be overriding implementations
                                ;; pick the earlier method (lowest in class hierarchy)
                                most-specific)))))]
                (recur (inc i) r)))
            most-specific))))))

(defn get-methods-pretypes [self-class target-class static? method-name nargs]
  (into []
    (filter
      (fn [method]
        (and
          (name-matching-method? static? method-name method)
          (arity-matching-method? nargs method)
          (member-accessible? self-class target-class (-getModifiers method)))))
    (if static?
      (-getDeclaredMethods target-class)
      (get-all-instance-methods target-class))))

(comment
  (count (get-methods-pretypes Object java.nio.file.Path true "of" 1))
  (count (get-methods-pretypes Object io.lacuna.bifurcan.List true "from" 1)))

(defn get-ctor-pretypes [self-class target-class nargs]
  (into []
    (filter
      (fn [method]
        (and
          (arity-matching-method? nargs method)
          (member-accessible? self-class target-class (-getModifiers method)))))
    (-getDeclaredConstructors target-class)))

(defn match-method-type ^Type
  [{:keys [self-classname] :as env}
   classname static? method-name arg-types ret-type]
  (assert (every? some? arg-types)
    (pr-str classname static? method-name arg-types))
  (let [c (if (string? classname)
            (resolve-class env classname)
            classname)
        self-class (try (resolve-class env self-classname)
                     (catch ClassNotFoundException _ Object))
        ->c (fn [c] (if (satisfies? PClassNode c)
                      c (try (resolve-class env c)
                          (catch ClassNotFoundException _ Object))))
        arg-cs (mapv #(->c %) arg-types)
        ret-c (when ret-type (->c ret-type))
        matches
        (into [] (comp
                   (filter #(= (-getName %) method-name))
                   (filter #(= static?
                              (Modifier/isStatic (-getModifiers %))))
                   (filter #(= 0 (bit-and Opcodes/ACC_SYNTHETIC (-getModifiers %))))
                   (filter #(member-accessible? self-class c (-getModifiers %)))
                   (filter #(method-applicable? arg-cs ret-c % (-getRetType %)))
                   )
          (if static? (-getDeclaredMethods c)
            (get-all-instance-methods c)))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No method matches: " classname (if static? "/" ".") method-name
                   " " (str (vec (partitionv 2 (interleave arg-types (map -getName arg-cs)))))
                   " => " (pr-str ret-type))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getMethodType (type/classname->type (-getName (-getRetType m)))
              (into-array Type (mapv (comp type/classname->type -getName)
                                 (-getParamTypes m)))))
        (throw (RuntimeException.
                 (str "Method ambiguous: " classname (if static? "/" ".") method-name
                   " " (pr-str arg-types) " => " (pr-str ret-type)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(comment
  (match-method-type {:self-classname "java.lang.Object"}
    "java.lang.Integer" true "valueOf" ["double"] nil)
  (match-method-type
  {:self-classname "java.lang.Object"
   :class-resolver #'jl.kickstart/cof}
  "chic.sqeditor.Interactor" false "handle-mouseup"
  ["io.github.humbleui.jwm.EventMouseButton"] nil)

(first
  (filter #(= "start" (.getName %))
    (vec (get-all-instance-methods java.lang.Thread$Builder$OfVirtual)))))


(defn lookup-method-type
  ^Type [{:keys [new-classes] :as env} classname static? method-name arg-types ret-type]
  (match-method-type env classname static? method-name arg-types ret-type)
  #_(let [mthds (get-in new-classes
                [classname (if static? :class-methods :instance-methods)])
        mthd (first (filter #(= method-name (:name %)) mthds))]
    (if mthd
      (Type/getMethodType (type/classname->type (:ret-classname mthd))
        (into-array Type (map type/classname->type (:param-classnames mthd))))
      (match-method-type env classname static? method-name arg-types ret-type))))

(defn match-ctor-type ^Type [env classname arg-types]
  (assert (every? some? arg-types))
  (let [c (resolve-class env classname)
        ->c (fn [c] (or (try (util/primitive-name->class c)
                          (catch Exception _))
                      (try (resolve-class env c)
                        (catch ClassNotFoundException _ Object))))
        arg-cs (mapv #(->c %) arg-types)
        matches (into [] (comp
                           (filter #(member-accessible? Object c (-getModifiers %)))
                           (filter #(method-applicable? arg-cs nil % nil))
                           )
                  (-getDeclaredConstructors c))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No ctor matches: " classname " "
                   (pr-str (partition 2 (interleave arg-types arg-cs))))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getMethodType Type/VOID_TYPE
              (into-array Type (mapv (comp type/classname->type -getName)
                                 (-getParamTypes m)))))
        (throw (RuntimeException.
                 (str "Ctor ambiguous: " classname " " (pr-str arg-types)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(defn get-field-type ^Type [env asking-classname classname static? field-name]
  (let [cls (if (string? classname)
              (resolve-class env classname)
              classname)
        askcls (if asking-classname (try (resolve-class env asking-classname)
                                      (catch ClassNotFoundException _ Object))
                 Object)
        field (first (eduction
                       (filter #(= field-name (-getName %)))
                       (filter #(= static?
                                 (Modifier/isStatic (-getModifiers %))))
                       (filter #(member-accessible? askcls cls (-getModifiers %)))
                       (-getDeclaredFields cls)))]
    (if field
      (type/classname->type (-getName (-getType field)))
      (throw (ex-info "Could not find accessible field"
               {:classname classname :field-name field-name
                :asking-class asking-classname})))))

(defn lookup-field-type
  ^Type [{:keys [new-classes self-classname] :as env} classname static? field-name]
  (get-field-type env self-classname classname static? field-name)
  #_(let [cls (get new-classes classname)
        field (first (eduction
                       (filter #(= field-name (:name %)))
                       (filter #(= static? (contains? (:flags %) :static)))
                       ;; (filter ) TODO access check
                       (:fields cls)))
        field (or (when field
                    (let [cn (:classname field)]
                      (when (nil? cn) (throw (ex-info "classname nil for field"
                                               {:field field
                                                :field-name field-name
                                                :static? static?
                                                :class classname})))
                      (type/classname->type cn)))
                (get-field-type env self-classname classname static? field-name))]
    (if field
      field
      (throw (RuntimeException. "Could not find accessible field")))))

(defn find-common-ancestor [classes]
  ;; finds most specific common class (not interface)
  (let
    [clss (java.util.ArrayList. 5)
     nonnil-classes (remove nil-class? classes)
     _ (loop [cls (first nonnil-classes)]
         (when cls
           (.add clss cls)
           (recur (-getSuperclass cls))))
     ;; first class and all its superclasses
     clss (.toArray clss)
     aindexof (fn [a offset x]
                (loop [i (dec (alength a))]
                  (when (<= offset i)
                    (if (= x (aget a i))
                      i
                      (recur (dec i))))))
     *lb (volatile! 0)
     res (loop [classes (next nonnil-classes)]
           (if-some [c (first classes)]
             (if (loop [c c]
                   (if-some [i (aindexof clss @*lb c)]
                     (do (vreset! *lb i) true)
                     (if-some [s (-getSuperclass c)]
                       (recur s)
                       nil)))
               (recur (next classes))
               nil)
             (aget clss @*lb)))]
    (if (and (not= (count classes) (count nonnil-classes))
          (-primitive? res))
      (throw (ex-info "Cannot unify nil with primitives" {}))
      res)))

(comment
  (= String (find-common-ancestor [nil-class String]))
  (= String (find-common-ancestor [String nil-class]))
   (find-common-ancestor [Integer/TYPE nil-class])
  (find-common-ancestor [Integer/TYPE Void/TYPE])
  (= Object (find-common-ancestor [Integer Object]))
  (= Number (find-common-ancestor [Integer BigInteger]))
  (nil? (find-common-ancestor [Integer/TYPE Integer]))
  )

(defn get-interfaces-since-super [sub super]
  (loop [acc (transient #{})
         sub sub]
    (if-let [ifaces (and sub
                      (not= sub super)
                      (-getInterfaces sub))]
      (recur
        (reduce conj! acc ifaces)
        (-getSuperclass sub))
      (persistent! acc))))

;; want to unroll each class up to the common ancestor - intersecting excess interfaces
(defn intersect-classes [classes]
  (if-let [ca (find-common-ancestor classes)]
    (new-unreal-class nil ca
      (reduce (fn [acc c]
                (into acc (get-interfaces-since-super c ca)))
        #{} (remove nil-class? classes)))
    (throw (ex-info "No common class ancestor; cannot intersect"
             {:classes classes}))))

(comment
  (intersect-classes [Integer/TYPE Integer])
  )

;; fields, local variables, and formal parameters
#_(defn munge-unqualified-name [name]
  )

#_(def ^bytes method-munge-map
  (let [m {\. 1
           \/ 2
           \; 3
           \< 4
           \> 5
           \[ 6}
        ks (sort (map int (keys m)))
        low (first ks)
        high (last ks)
        a (byte-array (inc (- high low)))]
    (doseq [[c offset] m]
      (aset-byte a (- (int c) low) offset))
    a))
(defn method-char->munge-offset [c]
  (case c
    \. 1
    \/ 2
    \; 3
    \< 4
    \> 5
    \[ 6
    -1))
(defn method-munge-offset->char [c]
  (case c
    1 \.
    2 \/
    3 \;
    4 \<
    5 \>
    6 \[
    (char 0)))

(defn munge-method-name* [ary n start offset1]
  (let [sb (StringBuilder.)
        c1 ^char (first "\uDB80")
        c2 ^char (first "\uDC00")]
    (when (< 0 start)
      (.append sb ary 0 start))
    (loop [i (inc start)
           offset offset1]
      (.append sb c1)
      (.append sb (char (+ (int c2) offset)))
      (when-some
        [[i offset]
         (loop [i i]
           (when (< i n)
             (let [c (aget ary i)
                   offset (method-char->munge-offset c)]
               (if (<= 0 offset)
                 [(inc i) offset]
                 (do (.append sb c)
                   (recur (inc i)))))))] 
        (recur i offset)))
    (.toString sb)))

(defn munge-method-name [^String name]
  (let [a (.toCharArray name)
        n (alength a)]
    (loop [i 0]
      (if (< i n)
        (let [offset (method-char->munge-offset (aget a i))]
          (if (<= 0 offset)
            (munge-method-name* a n i offset)
            (recur (inc i))))
        name))))

(defn demunge-method-name* [ary n start start-c]
  (let [sb (StringBuilder.)
        esc-c1 ^char (first "\uDB80")
        base-c2 ^char (first "\uDC00")]
    (when (< 0 start)
      (.append sb ary 0 start))
    (loop [i (+ start 2)
           c start-c]
      (.append sb c)
      (when-some
        [[i c]
         (loop [i i]
           (when (< i n)
             (let [c1 (aget ary i)]
               (if (= c1 esc-c1)
                 (let [c2 (aget ary i)
                       offset (- (int c2) (int base-c2))
                       c (method-munge-offset->char offset)]
                   (if (<= 0 (int c))
                     [(inc i) c]
                     (do (.append sb c1)
                       (.append sb c2)
                       (recur (+ i 2)))))
                 (do (.append sb c1)
                   (recur (inc i)))))))] 
        (recur i c)))
    (.toString sb)))

(defn demunge-method-name [^String name]
  (let [a (.toCharArray name)
        n (alength a)
        esc-c1 ^char (first "\uDB80")
        base-c2 ^char (first "\uDC00")]
    (loop [i 0]
      (if (< i n)
        (let [c1 (aget a i)]
          (if (= c1 esc-c1)
            (let [c2 (aget a (inc i))
                  offset (- (int c2) (int base-c2))
                  c (method-munge-offset->char offset)]
              (if (<= 0 (int c))
                (demunge-method-name* a n i c)
                (recur (+ i 2))))
            (recur (inc i))))
        name))))

(comment
  (let [s "hi"]
    (identical? s (munge-method-name s)))
  (let [s "tap>"]
    (= s (demunge-method-name (munge-method-name s))))
  (let [s "a->b"]
    (= s (demunge-method-name (munge-method-name s))))
  (let [s ".x"]
    (= s (demunge-method-name (munge-method-name s))))
  )

(def jlc-essential
  #{"Boolean"
    "Byte"
    "Short"
    "Character"
    "Integer"
    "Long"
    "Float"
    "Double"
    "Object"
    "String"
    "Class"
    
    "Math"
    "StrictMath"
    "StringBuilder"
    
    "System"
    
    "Thread"
    "ThreadLocal"
    "InheritableThreadLocal"})

(def jlc-extra
  #{"CharSequence"
    "Number"
    "Void"
  
    ; "Package"
    "Enum"
    ; "Enum$EnumDesc"
  
    ; "ThreadGroup"
  
    ; "StringBuffer"
    ; "Character$Subset"
    ; "Character$UnicodeBlock"
    ; "Character$UnicodeScript"
  
    ; "Runtime"
    ; "Runtime$Version"
  
    "System$Logger"
    ; "System$LoggerFinder"
    "System$Logger$Level"
  
    ; "Thread$Builder"
    ; "Thread$Builder$OfVirtual"
    ; "Thread$Builder$OfPlatform"
    "Thread$UncaughtExceptionHandler"
    "Thread$State"
  
    "Iterable"
    "AutoCloseable"
    "Cloneable"
    "Comparable"
    "Cleanable"
    "Readable"
    "Runnable"
  
    "Process"
    ; "ProcessHandle"
    ; "ProcessHandle$Info"
    ; "ProcessBuilder$Redirect$Type"
  
    "StackWalker"
    "StackWalker$StackFrame"
    "StackWalker$Option"
    "StackTraceElement"
    "ClassLoader"
  
    "Throwable"
    "Exception"
    "ArithmeticException"
    "ArrayIndexOutOfBoundsException"
    "ArrayStoreException"
    "ClassCastException"
    "ClassNotFoundException"
    "CloneNotSupportedException"
    "EnumConstantNotPresentException"
    "IllegalAccessException"
    "IllegalArgumentException"
    "IllegalCallerException"
    "IllegalStateMonitorException"
    "IllegalStateException"
    "IllegalThreadStateException"
    "IndexOutOfBoundsException"
    "InstantiationException"
    "InterruptedException"
    "LayerInstantiationException"
    "NegativeArraySizeException"
    "NoSuchFieldException"
    "NoSuchMethodException"
    "NullPointerException"
    "NumberFormatException"
    "MatchException"
    "RuntimeException"
    "ReflectiveOperationException"
    "StringIndexOutOfBoundsException"
    "TypeNotPresentException"
    "UnsupportedOperationException"
    "WrongThreadException"
  
    "Error"
    "AbstractMethodError"
    "AssertionError"
    "BootstrapMethodError"
    "ClassCircularityError"
    "ClassFormatError"
    "ExceptionInInitializerError"
    "IllegalAccessError"
    "InternalError"
    "LinkageError"
    "NoClassDefFoundError"
    "NoSuchFieldError"
    "NoSuchMethodError"
    "StackOverflowError"
    "UnknownError"
    "UnsatisfiedLinkError"
    "UnsupportedClassVersionError"
    "VerifyError"
    "VirtualMachineError"
    })

(def base-class-aliases
  (into {"BigInteger" "java.math.BigInteger"
         "BigDecimal" "java.math.BigDecimal"}
    (map (fn [nam]
           [nam (str "java.lang." nam)]))
    (into jlc-extra jlc-essential)))

#_#{"ClassValue"
  
  "Record"
  
  "Module"
  "ModuleLayer"
  "ModuleLayer$Controller"}



