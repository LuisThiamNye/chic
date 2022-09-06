(ns jl.compiler.op
  (:require
    ; [jl.compiler.type :as type]
    [taoensso.encore :as enc])
  (:import
    (java.util WeakHashMap)
    (org.objectweb.asm Opcodes Type)))

(def kw->opcode-0
  (let [op-names (into #{} (map str) '[NOP, ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD, IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE, POP, POP2, DUP, DUP_X1, DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP, IADD, LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL, IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM, INEG, LNEG, FNEG, DNEG, ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F, I2B, I2C, I2S, LCMP, FCMPL, FCMPG, DCMPL, DCMPG, IRETURN, LRETURN, FRETURN, DRETURN, ARETURN, RETURN, ARRAYLENGTH, ATHROW, MONITORENTER, MONITOREXIT])]
    (into {} (keep (fn [^java.lang.reflect.Field fld]
                     (when-some [op (op-names (.getName fld))]
                       [(keyword (.replace (.toLowerCase ^String op)
                                   \_ \-))
                        (.getInt fld Opcodes)])))
      (.getFields Opcodes))))

;; variables
'[ILOAD, LLOAD, FLOAD, DLOAD, ALOAD
  ISTORE, LSTORE, FSTORE, DSTORE, ASTORE, RET]

(def ^WeakHashMap kw->opcode-whm (WeakHashMap.))
(defn kw->opcode [kw]
  (or (.get kw->opcode-whm kw)
    (let [f (-> (name kw) .toUpperCase (.replace \- \_))
          i (.getInt (.getField Opcodes f) Opcodes)]
      (.put kw->opcode-whm kw i)
      i)))

;; jumps
'[IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ, IF_ACMPNE, GOTO, JSR, IFNULL or IFNONNULL]

(defn type->opcode [^Type type]
  (enc/case-eval (.getSort type)
    Type/BOOLEAN Opcodes/T_BOOLEAN
    Type/BYTE Opcodes/T_BYTE
    Type/SHORT Opcodes/T_SHORT
    Type/CHAR Opcodes/T_CHAR
    Type/INT Opcodes/T_INT
    Type/LONG Opcodes/T_LONG
    Type/FLOAT Opcodes/T_FLOAT
    Type/DOUBLE Opcodes/T_DOUBLE))

(defn kw->acc-opcode [kw]
  (case kw
    :public Opcodes/ACC_PUBLIC
    :final Opcodes/ACC_FINAL
    :super Opcodes/ACC_SUPER
    :interface Opcodes/ACC_INTERFACE
    :abstract Opcodes/ACC_ABSTRACT 
    :synthetic Opcodes/ACC_SYNTHETIC 
    :annotation Opcodes/ACC_ANNOTATION
    :enum Opcodes/ACC_ENUM
    :module Opcodes/ACC_MODULE
    
    ;; members only
    :static Opcodes/ACC_STATIC
    :private Opcodes/ACC_PRIVATE))

(defn acc-flags->kws [flags]
  (reduce (fn [acc kw]
            (let [op (kw->acc-opcode kw)]
              (if (< 0 (bit-and op flags))
                (conj acc kw) acc)))
    #{} #{:public :final :super :interface :abstract
          :synthetic :annotation :enum :module :static
          :private}))

(comment (acc-flags->kws 0x001B))