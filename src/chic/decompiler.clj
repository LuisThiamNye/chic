(ns chic.decompiler
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io])
  (:import
   (org.objectweb.asm ClassVisitor MethodVisitor Opcodes)
   (org.objectweb.asm.tree ClassNode MethodNode)
   (clojure.lang Compiler)
   #_#_(com.strobel.decompiler Decompiler DecompilerSettings)
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
  (let [paths (filterv #(= "class" (fs/extension %)) (fs/glob tmp-dir "**"))]
    (if (== 1 (count paths))
      paths
      (filterv #(not= "result__init.class" (fs/file-name %)) paths))))

(comment
  (clean-tmp-dir)
  (compile-classfiles '(+ 1 2))

  (let [bytecode (fs/read-all-bytes (first (compiled-classfile-paths)))
        mn (MethodNode.)
        cv (proxy [ClassVisitor] [Opcodes/ASM4]
             (visitMethod [api mname & args]
               (when (= "load" mname)
                 mn)))
        _ (.accept (ClassReader. bytecode) cv 0)]
    (seq (.instructions mn)))


  (let [path (first (compiled-classfile-paths))
        w (com.strobel.decompiler.PlainTextOutput.)]
    (Decompiler/decompile
     (str path)
     w
     (doto (DecompilerSettings/javaDefaults)
       (.setLanguage (Languages/bytecode))))
    (chic.debug/println-main(str w)))

  #!
  )
