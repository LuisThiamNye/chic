(ns jl.interop
  (:require
    [chic.util :as util]
    [jl.compiler.op :as op]
    [jl.compiler.type :as type])
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
                        fields methods])
(extend-type UnrealClass
  PNamed
  (-getName [self] (:name self))
  PClassNode
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

(defn new-unreal-class
  ([name super interfaces]
   (->UnrealClass name super (object-array interfaces) 0 nil nil))
  ([name super interfaces opts]
   (into (->UnrealClass name super (object-array interfaces) 0
           (object-array (mapv map->UnrealField (:fields opts)))
           (object-array (mapv map->UnrealMethod (:methods opts))))
     (update (dissoc opts :fields :methods)
       :constructors #(object-array (mapv map->UnrealMethod %))))))

(defn array-class-ndims [cls]
  (loop [i 1
         c cls]
    (let [el (-elementType c)]
      (if (-arrayClass? el)
        (recur (inc i) el)
        i))))

(defn resolve-obj-class [{:keys [new-classes class-resolver]} classname]
  (or
    (get-in new-classes [classname :class])
    (try (util/primitive-name->class classname)
      (catch Exception _))
    (when (= "void" classname) Void/TYPE)
    (if-some [info (get new-classes classname)]
      (or (:class info) (throw (ex-info "classinfo has no :class"
                                 {:classname classname})))
      (try (or (when class-resolver (class-resolver classname))
             (find-class classname))
        (catch ExceptionInInitializerError e
          (throw (ClassNotFoundException. e)))))))

(defn dynamic-class? [env cls]
  (-hiddenClass? cls))

(defn split-classname [classname]
  (let [ndims (count (re-find #"^\[+" classname))
        el-classname (if (< 0 ndims)
                       (second (re-matches #"L(.+);" (subs classname ndims)))
                       classname)]
    [ndims el-classname]))

(defn array-class-of [el-class ndims]
  (loop [i 0
         c el-class]
    (if (< i ndims)
      (recur (inc i) (-arrayType c))
      c)))

(defn resolve-class [env classname]
  (let [[ndims el-classname] (split-classname classname)
        el-class (resolve-obj-class env el-classname)]
    (if (= 0 ndims)
      el-class
      (array-class-of el-class ndims))))

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
  (let [cls (resolve-class env classname)
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
  (let [clss (java.util.ArrayList. 5)
        _ (loop [cls (first classes)]
                 (when cls
                   (.add clss cls)
                   (recur (-getSuperclass cls))))
        clss (.toArray clss)
        aindexof (fn [a offset x]
                    (loop [i (dec (alength a))]
                      (when (<= offset i)
                        (if (= x (aget a i))
                          i
                          (recur (dec i))))))
        *lb (volatile! 0)]
    (doseq [c (next classes)]
      (loop [c c]
        (if-some [i (aindexof clss @*lb c)]
          (vreset! *lb i)
          (when-some [s (-getSuperclass c)]
            (recur s)))))
    (aget clss @*lb)))

(defn get-interfaces-since-super [sub super]
  (loop [acc (transient #{})
         sub sub]
    (if-let [ifaces (and (not= sub super)
                      (-getInterfaces sub))]
      (recur (reduce conj! acc ifaces)
        (-getSuperclass sub))
      (persistent! acc))))

;; want to unroll each class up to the common ancestor - intersecting excess interfaces
(defn intersect-classes [classes]
  (let [ca (find-common-ancestor classes)]
    (new-unreal-class nil ca
      (reduce (fn [acc c]
                (into acc (get-interfaces-since-super c ca)))
        #{} classes))))

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
    "BigInteger"
    "BigDecimal"
    
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
  (into {} (map (fn [nam]
                  [nam (str "java.lang." nam)]))
    (into jlc-extra jlc-essential)))

#_#{"ClassValue"
  
  "Record"
  
  "Module"
  "ModuleLayer"
  "ModuleLayer$Controller"}



