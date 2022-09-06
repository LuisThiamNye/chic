(ns jl.kickstart
  (:require
    [jl.compiler.core :as compiler]
    [jl.interop :refer [find-class]]
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

(do
  (def file->classnames
  {"src2/sq/lang/util.sq"
   ["sq.lang.util.TrimRefValueMapLoopRunner"
    "sq.lang.util.SilentThreadUncaughtExceptionHandler"]
   "src2/sq/lang/keyword.sq"
   ["sq.lang.i.Named"
    "sq.lang.Keyword"
    "sq.lang.KeywordMgr"
    "sq.lang.KeywordFactory"]
   "src2/sq/lang/bootstrap.sq"
   ["sq.lang.InternalDataContainer"
    "sq.lang.DynClassMethodCallSite"]})

(def classname->file
  (reduce (fn [acc [f cns]]
            (reduce (fn [acc cn]
                      (assoc acc cn f))
              acc cns))
    {} file->classnames)))

(defn uninstall-class [c]
  (some-> (first
            (filter #(= "\udb80\udc00uninstall" (.getName %))
              (.getMethods c)))
    (.invoke c (object-array 0))))

(def ^java.util.concurrent.ConcurrentHashMap *hidden-class-lookups
  (java.util.concurrent.ConcurrentHashMap.))

(defn find-hidden-class [name]
  (some-> ^java.lang.invoke.MethodHandles$Lookup
    (.get *hidden-class-lookups name) .lookupClass))

(defn analyse-defclass-node [clsname]
  (let [filename (classname->file clsname)
        _ (assert (string? filename) "defclass not found")
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
      (update (ana/inject-default-env dc-node)
        :node/env
        (fn [env]
          (-> env
            (update :class-aliases into class-aliases)
            (assoc :class-resolver
              (fn [classname]
                (or (find-hidden-class classname)
                  (find-class classname))))))))))

(defn load-class [clsname]
  (try (when-some [cold (try (find-class clsname)
                              (catch Throwable _))]
             (println "Uninstalling old class: " clsname)
             (uninstall-class cold))
      (let [ret (compiler/eval-ast
                  (analyse-defclass-node clsname))]
           ret)
      (catch Throwable e (.printStackTrace e) :error)))

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
            lookup (get-class-lookup host-class)
            ret (compiler/load-ast-classes-hidden
                  {:lookup lookup}
                  (analyse-defclass-node clsname))]
        (doseq [[c lk] ret]
          (.put *hidden-class-lookups c lk))
        ret)
      (catch Throwable e (.printStackTrace e) :error)))

(defn swinvalidate-dyncls []
  (let [sw (.get (rfield (find-class "sq.lang.InternalDataContainer") 'map)
             "dynclsSwitchPoint")]
    (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
      "dynclsSwitchPoint" (java.lang.invoke.SwitchPoint.))
    (java.lang.invoke.SwitchPoint/invalidateAll
      (into-array java.lang.invoke.SwitchPoint [sw]))))

(comment
  (load-class "sq.lang.InternalDataContainer")
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "hclassLookups" *hidden-class-lookups)
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "dynclsSwitchPoint" (java.lang.invoke.SwitchPoint.))
  
  (load-class "sq.lang.DynClassMethodCallSite")
  
  (load-class "sq.lang.i.Named")
  (load-class "sq.lang.Keyword")
  
  (load-hidden-as "sq.lang.Keyword" "sq.lang.KeywordMgr")
  (load-class "sq.lang.KeywordFactory")
 

 (try (sq.lang.KeywordFactory/from "x")
   (catch Throwable e
     (.printStackTrace e)))
  
  
  (swinvalidate-dyncls)
  
  
  (load-class "sq.lang.util.TrimRefValueMapLoopRunner")
  (load-class "sq.lang.util.SilentThreadUncaughtExceptionHandler")
  
  
  
   (new sq.lang.DynClassMethodCallSite nil nil nil nil)
  
  
  (rfield (find-hidden-class "sq.lang.KeywordMgr") 'rqthread)
  (rfield (Class/forName "sq.lang.Keyword") 'cache)
  (do (= (rcall (find-hidden-class "sq.lang.KeywordMgr") 'from "x")
        (rcall (find-hidden-class "sq.lang.KeywordMgr") 'from "x")))
  
  (println (chic.decompiler/decompile
             "tmp/eval.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/sq.lang.KeywordFactory.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/sq.lang.util.SilentThreadUncaughtExceptionHandler.class" :bytecode))
  
  
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

"
Note: problem with converting Keywords to a unique int is that the keyword might
get GC'd at some point and the int loses its mapping + risks collisions
"

