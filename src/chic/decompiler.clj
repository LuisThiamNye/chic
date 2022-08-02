(ns chic.decompiler
  (:require
   [chic.debug :as debug]
   [babashka.fs :as fs]
   [clojure.java.io :as io])
  (:import
   (org.objectweb.asm ClassVisitor MethodVisitor Opcodes ClassWriter)
   (org.objectweb.asm.tree ClassNode MethodNode)
   (clojure.lang Compiler)
   (com.strobel.decompiler Decompiler DecompilerSettings PlainTextOutput)
   (com.strobel.decompiler.languages Languages)))

(def tmp-dir (fs/path (System/getProperty "java.io.tmpdir") "chic-decompiler"))

(when-not (fs/exists? tmp-dir)
  (fs/create-dir tmp-dir))

(defn clean-tmp-dir []
  (fs/delete-tree tmp-dir)
  (fs/create-dir tmp-dir))

(defn compile-classfiles [expr]
  (let [;tmp-file (File/createTempFile "tmp-src" "" tmp-dir)
        rdr (java.io.StringReader. (binding [*print-meta* true]
                                     (pr-str expr)))]
    (binding [*compile-files* true
              *compile-path* (str tmp-dir)]
      (Compiler/compile (io/reader rdr) "result.clj" "result"))))

(defn compiled-classfile-paths []
  (filterv #(= "class" (fs/extension %)) (fs/glob tmp-dir "**")))

(defn decompile [path lang]
  (let [w (com.strobel.decompiler.PlainTextOutput.)]
    (Decompiler/decompile
     (str path)
     w
     (doto (DecompilerSettings/javaDefaults)
       (.setLanguage (case lang
                       :java (Languages/java)
                       :bytecode (Languages/bytecode)
                       :bytecode-ast (Languages/bytecodeAst)))))
    (str w)))

(defn decompile-clj-expr [lang form]
  (try (compile-classfiles form)
    (mapv (fn [path]
            [(str (fs/relativize tmp-dir path)) (decompile path lang)])
      (compiled-classfile-paths))
    (finally (clean-tmp-dir))))

(comment
  (clean-tmp-dir)
  (doseq [[p c] (decompile-clj-expr :java 
                  (quote
                    (deftype ReplClass [x]
                      java.lang.AutoCloseable
                      (close [self]
                        (type x)
                        (type (.-x self))
                        (.close self)))))]
    (println "-- FILE: " p)
    (println c))

  (let [path (first (compiled-classfile-paths))]
    (debug/println-main (decompile path :java)))

  (let [m (MethodNode. (+ Opcodes/ACC_PUBLIC)
                       "close"
                       "()V"
                       nil nil)
        clsname "chic/ui/ui3/ICmpt3"
        cn (ClassNode.)
        _ (do (set! (.-access cn)
                    (+ #_Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE Opcodes/ACC_ABSTRACT))
              (set! (.-version cn) Opcodes/V19)
              (set! (.-name cn) clsname)
              (set! (.-superName cn) "java/lang/Object")
              #_(.add (.-interfaces cn))
              (.add (.-methods cn) m))
        cw (ClassWriter. Opcodes/ASM9)]
    (.accept cn cw)
    (.defineClass (.getContextClassLoader (Thread/currentThread))
                  (clojure.string/replace clsname #"/" ".") (.toByteArray cw) nil))

  #!
  )

