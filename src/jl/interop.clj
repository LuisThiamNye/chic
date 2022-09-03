(ns jl.interop
  (:require
    [chic.util :as util]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)
    (java.lang.reflect Method Field Modifier Constructor Executable)))

"Accessible https://docs.oracle.com/javase/specs/jls/se18/html/jls-6.html#jls-6.6
 and applicable https://docs.oracle.com/javase/specs/jls/se18/html/jls-15.html#jls-15.12.1"

(defn member-accessible? [^Class c1 ^Class target mods]
  (or (Modifier/isPublic mods)
    (let [pack2 (.getPackage target)
          pack1 (.getPackage c1)]
      (or (= pack2 pack1)
        (when (Modifier/isProtected mods)
          (.isAssignableFrom target c1))))))

(defn method-applicable? [arg-cs ^Class ret-c ^Executable method ^Class method-ret]
  (and (or (nil? ret-c) (.isAssignableFrom method-ret ret-c))
    (let [pts (.getParameterTypes method)]
      (or (and (= (count pts) (count arg-cs))
            (every? identity
              (map #(.isAssignableFrom %1 %2)
                pts arg-cs)))
          (when (.isVarArgs method)
            (let [vararg-type (.getComponentType (last (.getParameterTypes method)))]
              (and (every? identity
                     (map #(.isAssignableFrom %1 %2)
                       (butlast pts) arg-cs))
                (every? #(.isAssignableFrom vararg-type %)
                  (subvec arg-cs (dec (count (.getParameterTypes method))))))))))))

(defn sig-specificity>= [sig1 sig2]
  (every? (fn [[^Class t1 ^Class t2]]
            (.isAssignableFrom t2 t1))
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

(defn match-method-type ^Type [classname static? method-name arg-types ret-type]
  (assert (every? some? arg-types))
  (let [c (Class/forName classname)
        ->c (fn [c] (or (try (util/primitive-name->class c)
                          (catch Exception _))
                      (try (Class/forName c)
                        (catch ClassNotFoundException _ Object)
                        (catch ExceptionInInitializerError _ Object))))
        arg-cs (mapv #(->c %) arg-types)
        ret-c (when ret-type (->c ret-type))
        matches (into [] (comp
                           (filter #(= (.getName ^Method %) method-name))
                           (filter #(= static?
                                     (Modifier/isStatic (.getModifiers %))))
                           (filter #(member-accessible? Object c (.getModifiers ^Method %)))
                           (filter #(method-applicable? arg-cs ret-c % (.getReturnType ^Method %)))
                           )
                  (.getMethods c)
                  #_(.getDeclaredMethods c))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No method matches: " classname (if static? "/" ".") method-name
                   " " (pr-str arg-types) " => " (pr-str ret-type))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getType ^Method m))
        (throw (RuntimeException.
                 (str "Method ambiguous: " classname (if static? "/" ".") method-name
                   " " (pr-str arg-types) " => " (pr-str ret-type)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(defn match-ctor-type ^Type [classname arg-types]
  (assert (every? some? arg-types))
  (let [c (Class/forName classname)
        ->c (fn [c] (or (try (util/primitive-name->class c)
                          (catch Exception _))
                      (try (Class/forName c)
                        (catch ClassNotFoundException _ Object)
                        (catch ExceptionInInitializerError _ Object))))
        arg-cs (mapv #(->c %) arg-types)
        matches (into [] (comp
                           (filter #(member-accessible? Object c (.getModifiers ^Constructor %)))
                           (filter #(method-applicable? arg-cs nil % nil))
                           )
                  (.getConstructors c)
                  #_(.getDeclaredConstructors c))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No ctor matches: " classname " " (pr-str arg-types))))
      (or (when-some [m (most-specific-method matches)]
            (Type/getType ^Constructor m))
        (throw (RuntimeException.
                 (str "Ctor ambiguous: " classname " " (pr-str arg-types)
                   "\nMatches: " (mapv #(.toString %) matches))))))))

(defn get-field-type ^Type [classname static? field-name]
  (let [cls (Class/forName classname)
        field (first (eduction
                       (filter #(= field-name (.getName ^Field %)))
                       (filter #(= static?
                                 (Modifier/isStatic (.getModifiers ^Field %))))
                       (filter #(member-accessible? Object cls (.getModifiers ^Field %)))
                       (.getDeclaredFields cls)))]
    (if field
      (Type/getType (.getType field))
      (throw (RuntimeException. "Could not find accessible field")))))

(defn lookup-field-type
  ^Type [{:keys [new-classes]} classname static? field-name]
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
                (get-field-type classname static? field-name))]
    (if field
      field
      (throw (RuntimeException. "Could not find accessible field")))))


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
    ; "RuntimePermission"
    "SecurityManager"
  
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
    "SecurityException"
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



