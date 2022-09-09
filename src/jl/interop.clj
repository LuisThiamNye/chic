(ns jl.interop
  (:require
    [chic.util :as util]
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

(defprotocol PClassNode
  (-interface? [_])
  (-satisfies? [_ super])
  (-getInterfaces [_])
  (-getSuperclass [_]))

(extend-type Class PClassNode
  (-interface? [self] (.isInterface self))
  (-getInterfaces [self]
    (.getInterfaces self))
  (-getSuperclass [self]
    (.getSuperclass self))
  (-satisfies? [self sup]
    ;; Class can only satisfy classes that already exist
    (when (class? sup)
      (.isAssignableFrom ^Class sup self))))

(defrecord UnrealClass [super interfaces ^int flags fields methods]
  PClassNode
  (-interface? [_] (< 0 (bit-and flags Opcodes/ACC_INTERFACE)))
  (-getInterfaces [_] interfaces)
  (-getSuperclass [_] super)
  (-satisfies? [self other]
    ;; could also implement by keeping set of visited bases and only checking the unvisited
    (or (identical? self other)
      (-satisfies? super other)
      (some #(-satisfies? % other)
        interfaces))))

(defn new-unreal-class [super interfaces]
  (->UnrealClass super interfaces 0 nil nil))

(defn dynamic-class? [env cls]
  (when (and (class? cls) (.isHidden ^Class cls))
    true))

(defn member-accessible? [^Class c1 ^Class target mods]
  (or (Modifier/isPublic mods)
    (let [pack2 (.getPackage target)
          pack1 (.getPackage c1)]
      ;; Note: packages are specific to a classloader.
      ;; this is too generous
      (or true ;(= (.getName pack2) (.getName pack1))
        (when (Modifier/isProtected mods)
          (.isAssignableFrom target c1))))))

(defn method-applicable? [arg-cs desired-ret-c ^Executable method method-ret-c]
  (and (or (nil? desired-ret-c)
         (-satisfies? method-ret-c desired-ret-c))
    (let [pts (.getParameterTypes method)]
      (or (and (= (count pts) (count arg-cs))
            (every? identity
              (map #(-satisfies? %1 %2)
                arg-cs pts)))
          (when (and (.isVarArgs method) (<= (count pts) (count arg-cs)))
            (let [vararg-type (.getComponentType (last pts))]
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
  (reduce (fn [^Method ms ^Method method]
            (let [sig1 (.getParameterTypes ms)
                  sig2 (.getParameterTypes method)]
              (cond
                (sig-specificity>= sig1 sig2) ms
                (sig-specificity>= sig2 sig1) method
                :else (reduced nil))))
    (first methods) (next methods)))

(defn resolve-class [{:keys [new-classes class-resolver]} classname]
  (or
    (get-in new-classes [classname :class])
    
    (try (util/primitive-name->class classname)
      (catch Exception _))
    (try(or (when class-resolver (class-resolver classname))
          (find-class classname))
      (catch ClassNotFoundException _ Object)
      (catch ExceptionInInitializerError _ Object))))

(defn get-all-instance-methods [cls]
  (reify clojure.lang.IReduceInit
    (reduce [_ rf acc]
      (let
        [seen (java.util.HashSet.)
         rim (fn [acc cls]
              (let [ms (.getDeclaredMethods cls)]
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
               (let [ifaces (.getInterfaces cls)
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
              (let [ms (.getDeclaredMethods cls)
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
                      (if-some [s (.getSuperclass cls)]
                        (recur acc s)
                        acc))))))
         acc (if (.isInterface cls)
               (ri acc cls)
               (rc acc cls))]
        (if (reduced? acc) @acc acc)))))

(defn match-method-type ^Type
  [{:keys [self-classname] :as env}
   classname static? method-name arg-types ret-type]
  (assert (every? some? arg-types)
    (pr-str classname static? method-name arg-types))
  (let [c (resolve-class env classname)
        self-class (resolve-class env self-classname)
        ->c (fn [c] (if (satisfies? PClassNode c)
                      c (resolve-class env c)))
        arg-cs (mapv #(->c %) arg-types)
        ret-c (when ret-type (->c ret-type))
        matches
        (into [] (comp
                   (filter #(= (.getName ^Method %) method-name))
                   (filter #(= static?
                              (Modifier/isStatic (.getModifiers %))))
                   (filter #(= 0 (bit-and Opcodes/ACC_SYNTHETIC (.getModifiers %))))
                   (filter #(member-accessible? self-class c (.getModifiers ^Method %)))
                   (filter #(method-applicable? arg-cs ret-c % (.getReturnType ^Method %)))
                   )
          (if static? (.getDeclaredMethods c)
            (get-all-instance-methods c)))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No method matches: " classname (if static? "/" ".") method-name
                   " " (pr-str (partition 2 (interleave arg-types arg-cs))) " => " (pr-str ret-type))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getType ^Method m))
        (throw (RuntimeException.
                 (str "Method ambiguous: " classname (if static? "/" ".") method-name
                   " " (pr-str arg-types) " => " (pr-str ret-type)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(comment
  (match-method-type
  {:self-classname "java.lang.Object"}
  "java.lang.Thread$Builder$OfVirtual" false "start"
  ["sq.lang.util.TrimRefValueMapLoopRunner"] nil)

(first
  (filter #(= "start" (.getName %))
    (vec (get-all-instance-methods java.lang.Thread$Builder$OfVirtual)))))


(defn lookup-method-type
  ^Type [{:keys [new-classes] :as env} classname static? method-name arg-types ret-type]
  (let [mthds (get-in new-classes
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
                      (try (find-class c)
                        (catch ClassNotFoundException _ Object)
                        (catch ExceptionInInitializerError _ Object))))
        arg-cs (mapv #(->c %) arg-types)
        matches (into [] (comp
                           (filter #(member-accessible? Object c (.getModifiers ^Constructor %)))
                           (filter #(method-applicable? arg-cs nil % nil))
                           )
                  (.getDeclaredConstructors c))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No ctor matches: " classname " " (pr-str arg-types))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getType ^Constructor m))
        (throw (RuntimeException.
                 (str "Ctor ambiguous: " classname " " (pr-str arg-types)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(defn get-field-type ^Type [asking-classname classname static? field-name]
  (let [cls (find-class classname)
        askcls (if asking-classname (find-class asking-classname) Object)
        field (first (eduction
                       (filter #(= field-name (.getName ^Field %)))
                       (filter #(= static?
                                 (Modifier/isStatic (.getModifiers ^Field %))))
                       (filter #(member-accessible? askcls cls (.getModifiers ^Field %)))
                       (.getDeclaredFields cls)))]
    (if field
      (Type/getType (.getType field))
      (throw (ex-info "Could not find accessible field"
               {:classname classname :field-name field-name
                :asking-class asking-classname})))))

(defn lookup-field-type
  ^Type [{:keys [new-classes self-classname]} classname static? field-name]
  (let [cls (get new-classes classname)
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
                (get-field-type self-classname classname static? field-name))]
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
    (new-unreal-class ca
      (reduce (fn [acc c]
                (into acc (get-interfaces-since-super c ca)))
        #{} classes))))


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



