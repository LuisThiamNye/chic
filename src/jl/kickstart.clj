(ns jl.kickstart
  (:require
    [jl.compiler.core :as compiler]
    [jl.reader :as reader]
    [jl.compiler.analyser :as ana]))

(defn reflect-find-field* [fld flds]
  (reduce (fn [_ ^java.lang.reflect.Field f]
            (when (= fld (.getName f))
              (reduced f)))
    nil flds))
(defn rfield [target fld]
  (let [fld (name fld)
        cls (if (class? target) target (.getClass target))
        fld ^java.lang.reflect.Field
        (or (reflect-find-field* fld (.getFields cls))
          (reflect-find-field* fld (.getDeclaredFields cls)))]
    (.setAccessible fld true)
    (.get fld cls)))

(defn find-method-with-arity [method-name static? n methods]
  (let [[a b :as results]
        (filter #(and (= n (.getParameterCount %))
                   (= method-name (.getName %))
                   (= static?
                     (java.lang.reflect.Modifier/isStatic
                       (.getModifiers %)))) methods)]
    (if b
      (throw (ex-info "Overloaded method"
               {:matches results}))
      a)))

(defn rcall [target method-name & args]
  (let [method-name (name method-name)
        static? (class? target)
        cls (if static? target (.getClass target))
        method ^java.lang.reflect.Method 
        (first
          (keep
            (fn [cls] (find-method-with-arity method-name
                        static? (count args) (.getDeclaredMethods cls)))
            (take-while some? (iterate (fn [cls] (.getSuperclass cls)) cls))))]
    (.setAccessible method true)
    (.invoke method target (object-array args))))


(def classname->file
  {"sq.lang.util.TrimRefValueMapLoopRunner"
   "src2/sq/lang/util.sq"
   "sq.lang.util.SilentThreadUncaughtExceptionHandler"
   "src2/sq/lang/util.sq"
   "sq.lang.i.Named"
   "src2/sq/lang/keyword.sq"
   "sq.lang.Keyword"
   "src2/sq/lang/keyword.sq"
   "sq.lang.KeywordMgr"
   "src2/sq/lang/keyword.sq"})

(defn uninstall-class [c]
  (some-> (first
            (filter #(= "\udb80\udc00uninstall" (.getName %))
              (.getMethods c)))
    (.invoke c (object-array 0))))

(defn analyse-defclass-node [clsname]
  (let [filename (classname->file clsname)
        _ (assert (string? filename))
        contents (slurp filename)
        asts (ana/str->ast contents)
        [dc-node class-aliases]
        (reduce (fn [[_ class-aliases :as acc] node]
                  (let [[sym & args]
                        (and (= :list (:node/kind node))
                          (let [{[c1 :as children] :children} node]
                            (when (= :symbol (:node/kind c1))
                              children)))]
                    (if (nil? sym)
                      acc
                      (cond (and (= "defclass" (:string sym))
                              (= :symbol (:node/kind (first args)))
                              (= clsname (:string (first args))))
                        (reduced [node class-aliases])
                        (= "Alias-Classes" (:string sym))
                        [nil (into class-aliases
                               (partitionv 2 (map :string args)))]
                        :else acc))))
          [nil {}] asts)]
    (ana/-analyse-node
      (update-in (ana/inject-default-env dc-node)
        [:node/env :class-aliases] into class-aliases))))

(defn find-class [name]
  ;; Ensure class initialises via its own classloader
  ;; Otherwise, if using callers' CL, it won't see updated classes
  ;; of the same name
  (let [c (Class/forName name false (clojure.lang.RT/baseLoader))]
    (Class/forName name true (.getClassLoader c))))

(defn load-class [clsname]
  (try (when-some [cold (try (find-class clsname)
                              (catch Exception _))]
             (println "Uninstalling old class: " clsname)
             (uninstall-class cold))
      (let [ret (compiler/eval-ast
                  (analyse-defclass-node clsname))]
           ret)
      (catch Throwable e (.printStackTrace e) :error)))

(def *hidden-class-lookups (atom {}))

(defn find-hidden-class [name]
  (some-> ^java.lang.invoke.MethodHandles$Lookup
    (get @*hidden-class-lookups name) .lookupClass))

(def ^java.util.WeakHashMap classloader-lookups (java.util.WeakHashMap.))
(ns-unmap *ns* 'hidden-class-lo)

(defn get-class-lookup [^Class cls]
  (locking classloader-lookups
    (let [cl (.getClassLoader cls)]
      (java.lang.invoke.MethodHandles/privateLookupIn cls
        (or (.get classloader-lookups cl)
          (let [lk (compiler/eval-ast
                     {:classloader cl}
                     (ana/-analyse-node
                       (ana/inject-default-env
                         (first
                           (ana/str->ast "
  (jc java.lang.invoke.MethodHandles lookup)")))))]
            (.put classloader-lookups cl lk)
            lk))))))

(defn load-hidden-as [host clsname]
  (try (when-some [lk (try (get @*hidden-class-lookups clsname)
                              (catch Exception _))]
         (println "Uninstalling old class: " clsname)
         (uninstall-class (.lookupClass lk))
         (swap! *hidden-class-lookups dissoc lk))
      (let [host-class (find-class host)
            _ (prn (.getClassLoader host-class))
            lookup (get-class-lookup host-class)
            ret (compiler/load-ast-classes-hidden
                  {:lookup lookup}
                  (analyse-defclass-node clsname))]
        (swap! *hidden-class-lookups into ret)   
        ret)
      (catch Throwable e (.printStackTrace e) :error)))

(comment
  (load-class "sq.lang.i.Named")
  (load-class "sq.lang.Keyword")
  (load-class "sq.lang.util.TrimRefValueMapLoopRunner")
  (load-class "sq.lang.util.SilentThreadUncaughtExceptionHandler")
  (load-hidden-as "sq.lang.Keyword" "sq.lang.KeywordMgr")
  
  ;; true loader:
  (prn (.getClassLoader (Class/forName "sq.lang.Keyword")))
  (def --fnloader (.getClassLoader (Class/forName "sq.lang.Keyword" true
                                     (.getClassLoader (class load-hidden-as)))))
  ;; the fn's classloader did not define sq.lang.Keyword
  ;; outdated loader:
  (prn --fnloader)
  ;; Class/forName calls loadClass https://stackoverflow.com/a/39768345/10496841 (and loadClass documentation)
  ;; bad:
  (prn (.getClassLoader (rcall --fnloader 'loadClass "sq.lang.Keyword" false)))
  ;; - implies findLoadedClass returns non-null
  ;; - implies "this loader has been recorded by the Java virtual machine as an initiating loader of a class with that binary name"
  ;; - perhaps this means that while Keyword was defined by the other CL,
  ;;   the fn's CL was the one that loaded it so the old Keyword gets priority over
  ;;   the new one due to Clojure's implementation of loadClass (checks findLoadedClass first)
  ;; a solution:
  (prn (.getClassLoader (Class/forName "sq.lang.Keyword" true
                          (clojure.lang.RT/makeClassLoader))))
  ;; works:
  (prn (.getClassLoader (rcall --fnloader 'findClass "sq.lang.Keyword")))
  (prn (.getClassLoader (rcall clojure.lang.DynamicClassLoader
                          'findInMemoryClass "sq.lang.Keyword")))
  (prn (.getClassLoader (.get (get (rfield clojure.lang.DynamicClassLoader 'classCache)
                                "sq.lang.Keyword"))))
  ;; cache is correct
  
  (prn (.getClassLoader (.lookupClass (get-class-lookup (Class/forName "sq.lang.Keyword")))))
  
  (prn (.getClassLoader (find-hidden-class "sq.lang.KeywordMgr")))
  
  "
https://github.com/openjdk/jdk/blob/6fc58b8324d5b2d36e8c62839eda50a16c9da7bd/src/hotspot/share/classfile/systemDictionary.cpp#L630
"
  
  
  (find-hidden-class "sq.lang.KeywordMgr")
  
  (type (get @*hidden-classes "sq.lang.KeywordMgr"))
  (get-class-lookup sq.lang.Keyword)
  
  
  
  (rfield (find-hidden-class "sq.lang.KeywordMgr") 'rqthread)
  (rfield (Class/forName "sq.lang.Keyword") 'cache)
  (do (= (rcall (find-hidden-class "sq.lang.KeywordMgr") 'from "x")
        (rcall (find-hidden-class "sq.lang.KeywordMgr") 'from "x")))
  
  
  (println (chic.decompiler/decompile
             "tmp/sq.lang.Keyword.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/sq.lang.util.SilentThreadUncaughtExceptionHandler.class" :bytecode))
  
  
  (.compareTo "a" "-")
  
  (.get (doto (.getDeclaredField sq.lang.Keyword "rqthread")
          (.setAccessible true))
    sq.lang.Keyword)
  
  
  (sq.lang.util.TrimRefValueMapLoopRunner.
    nil (java.util.concurrent.ConcurrentHashMap.))
  )

"
Packages are specific to a classloader. They are automatically created and provided
when a class is defined by the classloader. Only flexibility is that classloaders allow
defining packages beforehand, which the Cl will then apply to defined classes.

https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-5.html#jvms-5.3.6
Modules may be more flexible?
A module is bound to a single classloader.
Packages (not classes) are associated with modules.
A Package can only belong to one module.
Different Packages of a classloader can belong to different modules.
A Module belongs to exactly one module layer.
The layer's set of classloaders and modules are immutable.
Module can be modified to read additional modules, and add exports.
Every unnamed module reads every run-time module
Every unnamed module exports, to every run-time module, every run-time package associated with itself.

Seems unlikely that a classloader would associate a new class with a module defined by a different classloader.
Possible strategy could be to have one module per class, and control the reads/exports.

https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-5.html#jvms-5.4.4
There is no module-specific privacy for individual class members, only for entire classes.
Nest members are determined statically with the NestHost/NestMembers attributes.
Nest members must be in the same run-time package.

defineHiddenClass can be used to spin off a class 'considered' to have the same
run-time package, module.
Hidden classes can be added to the nest of the lookup class.

Note: ProtectionDomain not that useful now that SecurityManager API is
deprecated for removal
"

"
ConstantDesc for Keyword
Needs to be linked (interned), so not nominal. Should have KeywordDesc extends DynamicConstantDesc
Then Keyword implements Constable.
ASM does not appear to use ConstantDesc API, so not needed right now.
Also see ConstantDescs https://download.java.net/java/early_access/jdk19/docs/api/java.base/java/lang/constant/ConstantDescs.html
https://www.youtube.com/watch?v=iSEjlLFCS3E

For dynamic constants: https://www.youtube.com/watch?v=knPSQyUtM4I
prefer using ConstantBootstraps where possible.
ConstantBootstraps::invoke intended for adapting existing MHs
"

