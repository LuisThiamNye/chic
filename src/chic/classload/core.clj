(ns chic.classload.core
  (:require
    [babashka.fs :as fs]
    [potemkin :refer [doit]]
    [chic.util :as util :refer [<- loop-zip loopr]]
    [clojure.java.io :as io]
    [chic.util.impl.analyzer :as util.impl.ana :refer [mh-invoke]]
    [clojure.tools.deps.alpha :as deps])
  (:import
    (java.util Arrays)
    (java.lang ClassLoader)
    (java.lang.ref WeakReference ReferenceQueue Reference)
    (java.lang.invoke MethodHandles$Lookup MethodHandle MethodHandles MethodType)
    (java.net URLClassLoader URL)
    (clojure.lang RT DynamicClassLoader)))

(defn system-classpath ^"[Ljava.lang.String;" []
  (.split (System/getProperty "java.class.path")
    (System/getProperty "path.separator")))

(defn classloader-chain 
  ([] (classloader-chain (RT/baseLoader)))
  ([cl] (eduction (take-while identity)
          (iterate #(.getParent ^ClassLoader %) cl))))

(defn classloader-urls ^"[Ljava.net.URL;" [cl]
  (when (instance? URLClassLoader cl)
    (.getURLs ^URLClassLoader cl)))

(defn system-classloader-paths
  "=> set<str>"
  ([] (system-classloader-paths (RT/baseLoader)))
  ([cl]
   (persistent!
     (transduce (comp (mapcat classloader-urls)
                  (map str))
       conj!
       (transient (set (system-classpath)))
       (classloader-chain cl)))))

(defn system-classloader-urls
  "=> set<URL>"
  ([] (system-classloader-paths (RT/baseLoader)))
  ([cl]
   (persistent!
     (transduce (mapcat classloader-urls)
       conj!
       (transient (set (system-classpath)))
       (classloader-chain cl)))))

(defn pathstr->url ^URL [path]
  (.toURL (.toURI (new java.io.File path))))

(defn url-same-file? [^URL url1 ^URL url2]
  (.sameFile url1 url2))


(def -bootstrap-loader 
  (URLClassLoader. (into-array URL [(pathstr->url "classes")])
    ;; classloader uses parents to find clojure classes
    (ClassLoader/getSystemClassLoader)))
; (.close -bootstrap-loader)
; (.close (get-main-loader))
  
(def ^:redef -ccl-class (.loadClass ^ClassLoader -bootstrap-loader 
                          "chic.classload.ComplexRootClassLoader"))
;(.getClassLoader -ccl-class)
;(.getFields -ccl-class)


(let [lk (MethodHandles/lookup)
      get-ml (. lk findStaticGetter 
               -ccl-class "mainLoader" -ccl-class)
      cls-urls (.arrayType URLClassLoader)
      get-ul (. lk findStaticGetter 
               -ccl-class "subLoaders" cls-urls)
      set-ul (. lk findStaticSetter
               -ccl-class "subLoaders" cls-urls)]
  (defn get-main-loader ^URLClassLoader [] (mh-invoke get-ml))
  (defn alter-loose-url-loaders [f]
    (locking -ccl-class
      (mh-invoke set-ul (f (mh-invoke get-ul))))))
  
(defn ensure-url-loaders [urls]
  (alter-loose-url-loaders 
    (fn [^objects active-loaders]
      (let [cl (get-main-loader)
            existing-urls (into (set (eduction (mapcat classloader-urls)
                                       active-loaders))
                            (system-classloader-urls))
            new-ucls (vec (eduction
                            (remove existing-urls)
                            (map #(URLClassLoader. (into-array URL [%]) cl))
                            urls))
            n+ (count new-ucls)]
        (if (= 0 n+)
          active-loaders
          (let [n1 (alength active-loaders)
                n2 (+ n1 n+)
                a2 (Arrays/copyOf active-loaders n2)]
            (loop-zip [ucl new-ucls]
              [i n1]
              (when (< i n2)
                (aset a2 i ucl)
                (recur (unchecked-inc-int i))) )
            a2))))))

;; could use references to track .closed and decomissioned loaders
; (def *classloader-refs (atom #{}))

; (def ^ReferenceQueue -classloader-refqueue
;   (new ReferenceQueue))

; (defn clear-invalid-classloader-refs []
;   (loop []
;     (when-some [ref (.poll -classloader-refqueue)]
;       (swap! *classloader-refs disj ref)
;       (recur))))

; (def ^objects leaf-classloader-refs (object-array 0))

(defn install-compelx-classloader! []
  (.setContextClassLoader (Thread/currentThread)
    ;; Wrap to prevent defining classes on main loader.
    ;; Probably should just not use singleton...
    (DynamicClassLoader. (get-main-loader))))

(comment
  (def --d '{net.cgrand/xforms {:git/url "https://github.com/cgrand/xforms"
                               :git/tag "v0.19.3"
                               :git/sha "f4ebaea"}})
  
  (run! prn (keys (System/getProperties)))
  
  (:libs (read-string (slurp (System/getProperty "clojure.basis"))))
  
  (def --b (deps/create-basis {:aliases [:dev :repl]
                               :extra --d}))
  
  (def --extra-paths
    (let [existing (system-classloader-paths)]
      (eduction (remove #(contains? existing %))
        (:classpath-roots --b))))
  
  ;; virtual th cl?
  
  #_(let [scl (ClassLoader/getSystemClassLoader)]
    (eduction
      (filter (fn [^Thread th]
                (seq (eduction (filter #(identical? % scl))
                       (classloader-chain (.getContextClassLoader th))))))
      (.keySet (Thread/getAllStackTraces))))
  (doseq [t *1]
    (prn (.getName (.getThreadGroup t)) " -- " (.getName t)))
  
  ;; probably best to target threads that are known to be good for the 
  ;; new classloader.
  
  (def --g1
    (let [ths (make-array Thread 80)
          _ (.enumerate (.getThreadGroup (Thread/currentThread))
              ths false)
          s (set (remove nil? ths))]
      s))
  
  (doseq [t (clojure.set/difference (set (.keySet (Thread/getAllStackTraces)))
              --g1)]
    (prn (.getName t)))
  
  (ensure-url-loaders (map pathstr->url --extra-paths))
  
  (.getURLs (get-main-loader))

  (alter-loose-url-loaders #(doto % (-> seq first .getURLs first prn)))
  
  (Class/forName "chic.classload.ComplexRootClassLoader")
  
  (seq (classloader-chain (.getClassLoader Compiler)))
  
  (take 3 (reverse (classloader-chain (RT/baseLoader))))
  
  
  
  
  (def -this-java-path
    (-> *ns* str Compiler/munge (. replace \. \/)
      (as-> s (fs/path "src" (fs/parent s) "jvm"))))
  
  (let [-javac (javax.tools.ToolProvider/getSystemJavaCompiler)
        -jc-file-manager (. -javac getStandardFileManager nil nil nil)
        cunits (.getJavaFileObjectsFromPaths -jc-file-manager
                   ^java.util.Collection
                   (identity [(fs/path -this-java-path "ComplexRootClassLoader.java")]))
        t (.getTask -javac
              nil -jc-file-manager nil ["-d" "classes"] nil cunits)]
      (when-not (.call t)
        (throw (ex-info "Compilation failure" {}))))

  )

