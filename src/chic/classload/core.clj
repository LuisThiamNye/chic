(ns chic.classload.core
  (:require
    [babashka.fs :as fs]
    [potemkin :refer [doit]]
    [chic.util :as util :refer [<- loop-zip loopr]]
    [clojure.java.io :as io]
    [chic.util.impl.analyzer :as util.impl.ana :refer [mh-invoke]]
    [clojure.tools.deps.alpha :as deps]
    [chic.debug.nrepl :as debug.nrepl])
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
  (defn get-loose-url-loaders [] (mh-invoke get-ul))
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

(defn install-complex-classloader! 
  []
  ;; Wrap to prevent defining classes on main loader.
  ;; Probably should just not use singleton...
  (let [cl (DynamicClassLoader. (get-main-loader))]
    (.setContextClassLoader (Thread/currentThread))
    (.bindRoot Compiler/LOADER cl)))


(comment
  (def --d '{rewrite-clj/rewrite-clj {:mvn/version "1.1.45"}})
  
  (run! prn (keys (System/getProperties)))
  
  (-> (read-string (slurp (System/getProperty "clojure.basis")))
    (:libs) (get 'nrepl/nrepl))
  
  (def --b (deps/create-basis {:aliases [:dev :repl]
                               ;:extra --d
                               :override-deps --d
                               }))
  
  (def --extra-paths
    (let [existing (system-classloader-paths)]
      (eduction (remove #(contains? existing %))
        (:classpath-roots --b))))
 
  ;; set nrepl thread class loaders
  (run! (fn [^Thread th]
          (future
            (.setContextClassLoader th
              (DynamicClassLoader. (get-main-loader)))))
    (chic.debug.nrepl/nrepl-session-threads))
  
  
  (ensure-url-loaders (map pathstr->url --extra-paths))

  (-> (get-loose-url-loaders) seq first .getURLs first prn)
  
  
  (count (seq (classloader-chain)))
  
  
  
  
  
  ;; Compile java
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

