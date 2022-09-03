(ns jl.kickstart
  (:require
    [jl.compiler.core :as compiler]
    [jl.reader :as reader]
    [jl.compiler.analyser :as ana]))

(def classname->file
  {"sq.lang.util.TrimRefValueMapLoopRunner"
   "src2/sq/lang/util.sq"
   "sq.lang.util.SilentThreadUncaughtExceptionHandler"
   "src2/sq/lang/util.sq"
   "sq.lang.IKeyword"
   "src2/sq/lang/keyword.sq"
   "sq.lang.Keyword"
   "src2/sq/lang/keyword.sq"})

(defn uninstall-class [c]
  (some-> (first
            (filter #(= "\udb80\udc00uninstall" (.getName %))
              (.getMethods c)))
    (.invoke c (object-array 0))))

(defn load-class [clsname]
  (let [filename (classname->file clsname)
        _ (assert (string? filename))
        contents (slurp filename)
        asts (ana/str->ast contents)
        dc-node
        (first
          (filter
            (fn [node]
              (and (= :list (:node/kind node))
                (let [{[c1 c2] :children} node]
                  (and
                    (= :symbol (:node/kind c1))
                    (= "defclass" (:string c1))
                    (= :symbol (:node/kind c2))
                    (= clsname (:string c2))))))
            asts))]
    (try (when-some [cold (try (Class/forName clsname)
                              (catch Exception _))]
             (println "Uninstalling old class: " clsname)
             (uninstall-class cold))
      (let [ret (compiler/eval-ast
                     (ana/-analyse-node
                       (ana/inject-default-env dc-node)))]     
           ret)
      (catch Throwable e (.printStackTrace e) :error))))

(comment
  (load-class "sq.lang.util.TrimRefValueMapLoopRunner")
  (load-class "sq.lang.IKeyword")
  (load-class "sq.lang.util.SilentThreadUncaughtExceptionHandler")
  (load-class "sq.lang.Keyword")
  
  (println (chic.decompiler/decompile
             "tmp/sq.lang.Keyword.class" :bytecode))
  (println (chic.decompiler/decompile
             "tmp/sq.lang.util.SilentThreadUncaughtExceptionHandler.class" :bytecode))
  
  (sq.lang.Keyword/from "key")
  
  (.get (doto (.getDeclaredField sq.lang.Keyword "rqthread")
          (.setAccessible true))
    sq.lang.Keyword)
  
  
  (sq.lang.util.TrimRefValueMapLoopRunner.
    nil (java.util.concurrent.ConcurrentHashMap.))
  )