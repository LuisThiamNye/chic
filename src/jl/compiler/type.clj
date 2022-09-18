(ns jl.compiler.type
  (:require
    [taoensso.encore :as enc])
  (:import
    (java.lang ClassValue)
    (org.objectweb.asm Type)))

#_(def ^ClassValue basic-sort-classvalue
  (proxy ClassValue [] []
    (computeValue [_ cls]
      (case (.getName cls)
        Boolean/TYPE Type/BOOLEAN
        Short/TYPE Type/SHORT
        Character/TYPE Type/CHAR
        Integer/TYPE Type/INT
        Long/TYPE Type/LONG
        Float/TYPE Type/FLOAT
        Double/TYPE Type/DOUBLE
        Void/TYPE Type/VOID
        (if (.isArray cls)
          Type/ARRAY
          Type/OBJECT)))))

(defn unbox [^Type atype]
  (let [n (.getInternalName atype)]
    (if (.startsWith n "java/lang/")
      (let [n2 (.substring n 10)]
        (condp = n2
          "Integer" Type/INT_TYPE
          "Boolean" Type/BOOLEAN_TYPE
          "Long" Type/LONG_TYPE
          "Float" Type/FLOAT_TYPE
          "Double" Type/DOUBLE_TYPE
          "Character" Type/CHAR_TYPE
          "Byte" Type/BYTE_TYPE
          "Void" Type/VOID_TYPE
          "Short" Type/SHORT_TYPE))
      (throw (UnsupportedOperationException.)))))

(defn prim-classname->type ^Type [n]
  (condp = n
    "int" Type/INT_TYPE
    "boolean" Type/BOOLEAN_TYPE
    "long" Type/LONG_TYPE
    "float" Type/FLOAT_TYPE
    "double" Type/DOUBLE_TYPE
    "void" Type/VOID_TYPE
    "char" Type/CHAR_TYPE
    "byte" Type/BYTE_TYPE
    "short" Type/SHORT_TYPE
    nil))

(defn obj-classname->type [^String n]
  (Type/getObjectType (.replace n \. \/)))

(defn classname->type [c]
  (or (prim-classname->type c)
    (obj-classname->type c)))

(defn box [^Type atype]
  (let [in (enc/case-eval (.getSort atype)
             Type/BOOLEAN "java/lang/Boolean"
             Type/BYTE "java/lang/Byte"
             Type/SHORT "java/lang/Short"
             Type/CHAR "java/lang/Character"
             Type/INT "java/lang/Integer"
             Type/LONG "java/lang/Long"
             Type/FLOAT "java/lang/Float"
             Type/DOUBLE "java/lang/Double"
             Type/VOID "java/lang/Void"
             nil)]
    (if in
      (Type/getObjectType in)
      atype)))

(defn prim? [^Type atype]
  (enc/case-eval (.getSort atype)
    (list Type/OBJECT Type/ARRAY Type/METHOD)
    false true))

(defn array-type ^Type [^Type type ndims]
  (let [sb (StringBuilder.)
        _ (dotimes [_ ndims]
            (.append sb \[))
        _ (.append sb (.getDescriptor type))
        adesc (.toString sb)]
    (Type/getType adesc)))

(defn get-classname ^String [^Type type]
  (if (prim? type)
    (.getClassName type)
    ;; because getClassName returns java.lang.Object[], but want [Ljava.lang.Object;
    (.replace (.getInternalName type) \/ \.)))

