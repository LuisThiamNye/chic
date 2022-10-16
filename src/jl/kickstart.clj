(ns jl.kickstart
  (:require
    [taoensso.encore :as enc]
    [jl.compiler.sforms]
    [jl.compiler.math]
    [jl.compiler.core :as compiler]
    [jl.interop :as interop :refer [find-class]]
    [jl.reader :as reader]
    [jl.compiler.analyser :as ana]
    [io.github.humbleui.core :as hui]
    [io.github.humbleui.core :as core])
  (:import
    (java.util.concurrent ConcurrentHashMap)
    (org.objectweb.asm ClassWriter Opcodes)
    (java.lang.invoke MethodHandles MethodHandle MethodHandles$Lookup)))

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
    (.get fld target)))

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
    "sq.lang.util.SilentThreadUncaughtExceptionHandler"
    "sq.lang.util.ClassReflect"
    "sq.lang.util.Ints2"
    "sq.lang.util.Ints3"
    "sq.lang.util.RopeCharacterIterator"
    "sq.lang.util.EmptyCharacterIterator"
    "sq.lang.util.RopeUtil"
    "sq.lang.util.IteratorIterable"
    "sq.lang.util.Maths"]
   
   "src2/sq/lang/keyword.sq"
   ["sq.lang.i.Named"
    "sq.lang.Keyword"
    "sq.lang.KeywordMgr"
    "sq.lang.KeywordFactory"]
   
   "src2/sq/lang/bootstrap.sq"
   ["sq.lang.InternalDataContainer"
    "sq.lang.DynClassMethodCallSite"
    "sq.lang.DynInstanceMethodCallSite"
    "sq.lang.DynConstructorCallSite"
    "sq.lang.EnumSwitchMapCallSite"
    "sq.lang.i.MethodHandleFinderLk"
    "sq.lang.DynGetFieldCallSite"
    "sq.lang.DynSetFieldCallSite"]
   
   "src2/sq/lang/classloader.sq"
   ["sq.lang.DynamicClassLoader"
    "sq.lang.OnceInitialisingClassLoader"
    "sq.lang.LoadedClassLookup"]
   
   "src2/sq/lang/globals.sq"
   ["sq.lang.GlobalCHM"]
   "src2/window.sq"
   ["chic.window.Main"
    "chic.window.i.PaintAndEventHandler"
    "chic.window.StdWinEventListener"]
   
   "src3/sq/compiler/reader.sq"
   ["sq.compiler.reader.IFormVisitor"
    "sq.compiler.reader.Reader"
    "sq.compiler.reader.Util"
    "sq.compiler.reader.TrackedCharReader"
    "sq.compiler.reader.IChReader"
    "sq.compiler.reader.Stock"]
   
   "src2/sqeditor.sq"
   ["chic.sqeditor.RectTools"
    "chic.sqeditor.IntrMgr"
    "chic.sqeditor.Interactor"
    "chic.sqeditor.Buffer"
    "chic.sqeditor.BufferOps"
    "chic.sqeditor.Misc"
    "chic.sqeditor.BreakNav"
    "chic.sqeditor.i.LineOffsetConverter"
    "chic.sqeditor.TextButtonList"
    "chic.sqeditor.Region"
    "chic.sqeditor.RegionOps"
    "chic.sqeditor.RegionPathOps"
    "chic.sqeditor.Selection"
    "chic.sqeditor.SelectionOps"
    "chic.sqeditor.View"
    "chic.sqeditor.EditorCommit"
    "chic.sqeditor.TextEditor"
    "chic.sqeditor.Window"
    "chic.sqeditor.UiRoot"]})

(def classname->file
  (reduce (fn [acc [f cns]]
            (reduce (fn [acc cn]
                      (assoc acc cn f))
              acc cns))
    {} file->classnames)))

(defn uninstall-class [c]
  (when-some
    [m (first
         (filter #(= "\udb80\udc00uninstall" (.getName %))
           (try (.getMethods c)
             (catch Throwable e
               (println "INFO: broken class, not uninstallable: " c)
               (println (.toString e))
               (println)))))]
    (println "Uninstalling old class: " c)
    (try (.invoke m c (object-array 0))
      (catch Throwable _
        (println "Warning: could not uninstall")))))

(def ^java.util.concurrent.ConcurrentHashMap
  *meta-classes
  (java.util.concurrent.ConcurrentHashMap.))


(defn find-hidden-class [name]
  (let [[ndims el-name] (interop/split-classname name)
        el-mc (.get *meta-classes el-name)
        el-cls
        (or (:impl-class el-mc)
          (some-> ^java.lang.invoke.MethodHandles$Lookup
            (:lookup el-mc) .lookupClass))]
    (when el-cls
      (if (< 0 ndims)
        (interop/array-class-of el-cls ndims)
        el-cls))))

(defn cof [classname]
  (or (find-hidden-class classname)
    (find-hidden-class (str "_." (.replace classname \. \,)))
    (find-class classname)))

(def get-file-asts
  (enc/memoize 500
    (fn [filename]
      (let [_ (assert (string? filename) "defclass file not found")
            contents (slurp filename)]
        (ana/str->ast contents)))))

(defn analyse-defclass-node [opts clsname]
  (let [asts (get-file-asts (classname->file clsname))
        [dc-node class-aliases]
        (reduce (fn [[_ class-aliases :as acc] node]
                  (let [[sym & args]
                        (when (= :list (:node/kind node))
                          (let [{[c1 :as children] :children} node]
                            (when (= :symbol (:node/kind c1))
                              children)))
                        expand (fn[c] (get class-aliases c c))]
                    (if (nil? sym)
                      acc
                      (cond (and (= "defclass" (:string sym))
                              (= :symbol (:node/kind (first args)))
                              (= clsname (:string (first args))))
                        (reduced [node class-aliases])
                        (= "Alias-Classes" (:string sym))
                        [nil (into class-aliases
                               (map (fn [[a b]]
                                      [a (expand b)]))
                               (partitionv 2 (map :string args)))]
                        (= "Refer-Classes" (:string sym))
                        [nil (into class-aliases
                               (mapcat (fn [{:keys [children]}]
                                         (let [pkg (:string (first children))
                                               names (map :string (rest children))]
                                           (map #(vector % (expand (str pkg "." %))) names))))
                               args)]
                        :else acc))))
          [nil (into (cond-> {} (:hidden? opts)
                       (assoc clsname
                         (str "_." (.replace clsname \. \,))))
                 (map (fn [classname]
                        [(.replace (.replace classname \, \.) "_." "")
                         classname]))
                 (keys *meta-classes))]
          asts)]
    (assert (some? dc-node) "defclass form not found")
    (ana/-analyse-node
      (update (ana/inject-default-env dc-node)
        :node/env
        (fn [env]
          (-> env
            (update :class-aliases into class-aliases)
            (assoc :class-resolver
              (fn [classname]
                (or (.get *meta-classes classname)
                  (try (find-class classname)
                    (catch ClassNotFoundException _)))))))))))

(def ^java.util.WeakHashMap classloader-lookups (java.util.WeakHashMap.))

(defn get-classloader-lookup [cl classname]
  ;; workaround to obtain full privilege access to unnamed module of cl
  (locking classloader-lookups
    (or (.get classloader-lookups cl)
      (let [lk (compiler/eval-ast
                 {:classloader cl
                  :classname classname}
                 (ana/-analyse-node
                   (ana/inject-default-env
                      (first
                        (ana/str->ast "
(jc java.lang.invoke.MethodHandles lookup)")))
                   ))]
        (.put classloader-lookups cl lk)
        lk))))

(defn get-class-lookup [^Class cls]
  (java.lang.invoke.MethodHandles/privateLookupIn cls
    (get-classloader-lookup (.getClassLoader cls)
      (str (.getName (.getPackage cls)) ".(LookupInit)"))))

(def *current-hidden-class-loader (atom nil))

(defn refresh-hidden-class-loader! []
  (locking classloader-lookups
    (let [classname "_.LookupInit"
          ba (compiler/ir->eval-bytes
               {:classname classname}
               (ana/-analyse-node
                 (ana/inject-default-env
                   (first
                     (ana/str->ast "
(jc java.lang.invoke.MethodHandles lookup)")))
                 ))
          cl (clojure.lang.RT/makeClassLoader)
          cls (.defineClass cl classname ba nil)]
      (.put classloader-lookups cl (rcall cls 'run))
      (reset! *current-hidden-class-loader cl))))

(refresh-hidden-class-loader!)

(defn get-hidden-class-host-lookup []
  (.get classloader-lookups @*current-hidden-class-loader))

(defn load-class [clsname]
  (try (when-some [cold (try (find-class clsname)
                              (catch Throwable _))]
             (uninstall-class cold))
      (let [ir (analyse-defclass-node {} clsname)
            ret (compiler/eval-ast ir)
            mc (get-in ir [:node/env :new-classes clsname :class])]
        (assert (some? mc))
        (refresh-hidden-class-loader!)
        (.put *meta-classes clsname (assoc mc :impl-class (find-class clsname)))
        ret)
      (catch Throwable e (.printStackTrace e) :error)))

(defn swinvalidate-dyncls []
  (let [sw (.get (rfield (find-class "sq.lang.InternalDataContainer") 'map)
             "dynclsSwitchPoint")]
    (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
      "dynclsSwitchPoint" (java.lang.invoke.SwitchPoint.))
    (java.lang.invoke.SwitchPoint/invalidateAll
      (into-array java.lang.invoke.SwitchPoint [sw]))))

(defn load-hidden-as [host clsname]
  (try (when-some [c (try (or (find-hidden-class clsname)
                             (find-class clsname))
                        (catch Exception _))]
         (uninstall-class c)
         (.remove *meta-classes clsname))
      (let [lookup (if host
                     (get-class-lookup (find-class host))
                     (get-hidden-class-host-lookup))
            ir (analyse-defclass-node
                 {:hidden? (not host)}
                 clsname)
            ret (compiler/load-ast-classes-hidden
                  {:lookup lookup} ir)
            ;hclsname (or host (str "_." (.replace clsname \. \,)))
            new-classes (get-in ir [:node/env :new-classes])]
        (doseq [[c lk] ret]
          (let [mc (:class (get new-classes c))]
            (assert (some? mc))
            (.put *meta-classes c (assoc mc :lookup lk))))
        (swinvalidate-dyncls)
        ret)
      (catch Throwable e (.printStackTrace e) :error)))

(defn load-hidden [classname]
  (load-hidden-as nil classname))

(defn get-globalchm []
  (rfield (find-class "sq.lang.GlobalCHM") 'map))

(defn find-class-lk [lk classname]
  (or (when-some [h(find-hidden-class classname)]
        h)
    (try (find-class classname)
      (catch ClassNotFoundException _))
    (case classname
      "int" Integer/TYPE
      "long" Long/TYPE
      "boolean" Boolean/TYPE
      "short" Short/TYPE
      "char" Character/TYPE
      "void" Void/TYPE
      "float" Float/TYPE
      "double" Double/TYPE
      "byte" Byte/TYPE
      nil)
    (throw (ex-info "No impl class" {:classname classname}))))

(def classFinderLk
  (reify java.util.function.BiFunction
    (apply [_ lk classname]
      (find-class-lk lk classname))))

(defn find-method-handle-lk
  [^MethodHandles$Lookup lk target-cn static? method-name param-cns]
  (let [param-types (mapv (comp
                            (fn [^Class c]
                              (if (.isHidden c)
                                (if (.isArray c)
                                  (Class/forName "[Ljava.lang.Object;")
                                  Object)
                                c))
                            (partial find-class-lk lk))
                      param-cns)
        target-class (find-class-lk lk target-cn)
        lookup-class (.lookupClass lk)
        methods0 (interop/get-methods-pretypes lookup-class target-class
                   static? method-name (count param-types))
        method
        (case (count methods0)
          0 (throw (ex-info "No new matching methods found"
                     {:target-class target-class
                      :static? static?
                      :method-name method-name
                      :param-types param-types
                      :lookup-class lookup-class}))
          1 (nth methods0 0)
          (let [ms (interop/match-method-by-arg-types methods0 param-types)]
            (if ms
              ms
              (throw (ex-info "method ambiguous"
                       {:target-class target-class
                        :methods methods0
                        :static? static?
                        :method-name method-name
                        :arg-types param-types
                        :lookup-class lookup-class})))))]
    (.unreflect lk method)))

(load-class "sq.lang.i.MethodHandleFinderLk")

(def methodHandleFinderLk
  (reify sq.lang.i.MethodHandleFinderLk
    (apply [_ lk target-cn static? method-name param-cns]
      (find-method-handle-lk lk target-cn static? method-name param-cns))))

(comment
  (load-class "sq.lang.InternalDataContainer")
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "classFinderLk" classFinderLk)
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "methodHandleFinderLk" methodHandleFinderLk)
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "dynclsSwitchPoint" (java.lang.invoke.SwitchPoint.))
  (.put (rfield (find-class "sq.lang.InternalDataContainer") 'map)
    "clsCache" (ConcurrentHashMap.))
  
  (load-class "sq.lang.util.TrimRefValueMapLoopRunner")
  (load-class "sq.lang.util.SilentThreadUncaughtExceptionHandler")
  (load-class "sq.lang.util.Ints2")
  (load-class "sq.lang.util.Ints3")
  (load-class "sq.lang.GlobalCHM")
  ; (load-class "sq.lang.LoadedClassLookup")
  (load-class "sq.lang.util.ClassReflect")
  (load-hidden "sq.lang.util.RopeCharacterIterator")
  (load-hidden "sq.lang.util.EmptyCharacterIterator")
  (load-hidden "sq.lang.util.RopeUtil")
  (load-hidden "sq.lang.util.IteratorIterable")
  
  (load-class "sq.lang.DynClassMethodCallSite")
  (load-class "sq.lang.DynInstanceMethodCallSite")
  (load-class "sq.lang.DynConstructorCallSite")
  (load-class "sq.lang.DynGetFieldCallSite")
  (load-class "sq.lang.DynSetFieldCallSite")
  (load-class "sq.lang.EnumSwitchMapCallSite")
  
  (load-class "sq.lang.i.Named")
  (load-class "sq.lang.Keyword")
  (load-hidden-as "sq.lang.Keyword" "sq.lang.KeywordMgr")
  (load-hidden "sq.lang.util.Maths")
  
  (load-class "sq.lang.KeywordFactory")
  
  ;; READER
  
  (load-class "sq.compiler.reader.IFormVisitor")
  (load-class "sq.compiler.reader.IChReader")
  (load-hidden "sq.compiler.reader.Util")
  (load-hidden "sq.compiler.reader.Reader")
  (load-hidden "sq.compiler.reader.TrackedCharReader")
  (load-hidden "sq.compiler.reader.Stock")
  
  
  
  ;; SQEDITOR
  
  (.put (get-globalchm) "chic.window"
    (new java.util.concurrent.ConcurrentHashMap))
  (.put (.get (get-globalchm) "chic.window") "windows"
    (new java.util.concurrent.atomic.AtomicReference
      (new io.lacuna.bifurcan.Set)))
  (load-class "chic.window.i.PaintAndEventHandler")
  (load-hidden "chic.window.StdWinEventListener")
  (load-hidden "chic.window.Main")
  
  (load-hidden "chic.sqeditor.RectTools")
  (load-hidden "chic.sqeditor.Interactor")
  (load-hidden "chic.sqeditor.IntrMgr")
  
  (load-hidden "chic.sqeditor.Region")
  (load-hidden "chic.sqeditor.EditorCommit")
  (load-hidden "chic.sqeditor.Buffer")
  (load-hidden "chic.sqeditor.Misc")
  (load-hidden "chic.sqeditor.BreakNav")
  (load-class "chic.sqeditor.i.LineOffsetConverter")
  (load-hidden "chic.sqeditor.RegionOps")
  (load-hidden "chic.sqeditor.RegionPathOps")
  (load-hidden "chic.sqeditor.Selection")
  (load-hidden "chic.sqeditor.BufferOps")
  (load-hidden "chic.sqeditor.SelectionOps")
  (load-hidden "chic.sqeditor.TextButtonList")
  (load-hidden "chic.sqeditor.View")
  (load-hidden "chic.sqeditor.TextEditor")
  
  (load-hidden "chic.sqeditor.UiRoot")
  (load-hidden "chic.sqeditor.Window")
  
  (io.github.humbleui.skija.Typeface/makeFromName
    "Input SansCondensed" io.github.humbleui.skija.FontStyle/NORMAL)

  (chic.windows/dosendui
    (try (rcall (cof "chic.sqeditor.Window") 'spawn)
      (catch Throwable e
        (.printStackTrace e))))
  
  ;; Classes keep vanishing from behind the SoftReferences of DCL's cache
  ;; IllegalAccessErrors on classes happens after running this
  (run! (fn [c]
          (.put (rfield clojure.lang.DynamicClassLoader 'classCache)
            (.getName c) (java.lang.ref.SoftReference. c)))
    (eduction
      (keep (fn [{:keys [^Class impl-class]}]
              (when (and impl-class (not (.isHidden impl-class)))
                impl-class)))
      (vals *meta-classes)))
  
  (for [w (vec (.get (.get (rfield (cof "chic.window.Main") 'pkgmap) "windows")))]
    (chic.windows/safe-doui
      (.close (rfield w 'jwm-window))))

  
  (let [cn "sq.lang.DynInstanceMethodCallSite"]
    )
  
 (try (sq.lang.KeywordFactory/from "x")
   (catch Throwable e
     (.printStackTrace e)))
  (cof "chic.sqeditor.View")
  
  (swinvalidate-dyncls)
  (refresh-hidden-class-loader!)
    
  (require 'chic.decompiler)
  (println (chic.decompiler/decompile
             "tmp/eval.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/chic.sqeditor.TextEditor.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/sq.lang.DynClassMethodCallSite.class" :bytecode))
  (import '(io.lacuna.bifurcan Rope))
  

  
  ;; TODO mechanism for safely redefining class, preserving static fields
  (io.github.humbleui.jwm.Key/values)

  
  
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
(comment
  ;;
  ;; Class garbage collection example
  ;;
  (let [cn (str (gensym "test.Tmp"))
        _ (do (compiler/create-stub-class {:classname cn}) nil)
        dcl (clojure.lang.RT/baseLoader)
        exists? (fn []
                  (let [cc (rfield dcl 'classCache)
                        rq (rfield dcl 'rq)]
                    (clojure.lang.Util/clearCache rq cc)
                    (.containsKey cc cn)))]
    (assert (exists?))
    (System/gc)
    (exists?))
  
  (let [cn (str (gensym "test.Tmp"))
        ; c 
        c (do (compiler/create-stub-class {:classname cn}) nil)
        dcl (clojure.lang.RT/makeClassLoader)
        cl (sq.lang.OnceInitialisingClassLoader. dcl)
        exists? (fn []
                  (let [cc (rfield dcl 'classCache)
                        rq (rfield dcl 'rq)]
                    (clojure.lang.Util/clearCache rq cc)
                    (.containsKey cc cn)))]
    ;; c is definitely initialised
    (Class/forName cn true (clojure.lang.RT/makeClassLoader))
    (assert (exists?))
    ;; load class: must use this, not directly loadClass
    (Class/forName cn false cl)
    ;; cl is now an initiating loader
    (assert (some? (.findLoadedKlass cl cn)))
    (assert (exists?))
    ;; cl still alive, so c cannot be collected
    (System/gc)
    (assert (exists?))
    (assert (some? cl))
    ;; cl can be collected, c gets collected, dcl still alive
    (System/gc)
    (assert (not (exists?)))
    (assert (some? dcl)))
  ;; Conclusion: class can only be unloaded if all its initiating loaders are
  ;; unreachable. Makes sense when considering first rule of good classloader:
  ;; must always return same class given the same name.
  
  ;; No limit to how many classes the Cl can initiate and no gc happens
  ;; (verified via visualvm)
  (def --cl (sq.lang.OnceInitialisingClassLoader. (clojure.lang.RT/makeClassLoader)))
  (dotimes [_ 1000]
    (let [cn (str (gensym "test.Tmp"))]
      (compiler/create-stub-class {:classname cn})
      (Class/forName cn false --cl)
      nil))
  (let [dcl (clojure.lang.DynamicClassLoader.)
        cc (rfield dcl 'classCache)
        rq (rfield dcl 'rq)]
    (clojure.lang.Util/clearCache rq cc)
    (System/gc))
  ;; all classes can be reclaimed after running this
  (def --cl nil)
)


