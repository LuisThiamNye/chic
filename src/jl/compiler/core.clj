(ns jl.compiler.core
  (:require
    [taoensso.encore :as enc]
    [clojure.core.match :refer [match]]
    [chic.util :refer [deftype+ loop-zip]]
    [jl.interop :as interop]
    [jl.compiler.type :as type]
    [jl.compiler.op :as op]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as compiler])
  (:import
    (java.lang.invoke MethodHandles MethodHandles$Lookup$ClassOption MethodHandles$Lookup)
    (org.objectweb.asm ClassVisitor MethodVisitor Opcodes ClassWriter Type Label
      Handle ConstantDynamic)))

(defn static-classname [env classname]
  (let [cls (interop/resolve-class env classname)]
    (if (and (interop/dynamic-class? env cls)
          ;; can't seem to embed self in descriptor of indy for hidden class
          #_(not= classname (:self-classname env)))
      ;; vararg needs array type
      (if (interop/-arrayClass? cls)
        (str (apply str (repeat (interop/array-class-ndims cls) "[")) "Ljava.lang.Object;")
        "java.lang.Object")
      ; "java.lang.Object"
      classname)))

(defn as-impl-type ^Type [env type]
  (type/classname->type (static-classname env (type/get-classname type))))

"
JVM Local variales
- each method stack frame has a fixed allocation of slots.
- long consumes two slots
- slots can be reused regardless of type

"
#_
(defn prim-let-sf [ctx form]
  (let [bindpairs (partitionv 2 (nth form 1))
        body (subvec form 2)]
    (doseq [[sym vexpr] bindpairs]
      (-emitForm ctx vexpr)
      (-emitStore ctx sym))
    (-emitBody ctx body)))

"Specialised ifs - branches ahead if condition true

Binary (int):
if_acmpeq - = references
if-acmpne - not= references
if_icmpeq - =
if_icmpge - >=
if_icmpgt - >
if_icmple - <=
if_icmplt - <
if_icmpne - not=
Unary (int):
ifeq - =0
ifge - >=0
ifgt - >0
ifle - <=0
iflt - <0
ifne - not=0
ifnonnull
ifnull

Java compiles:
if (x==y) { A } else { B }

ifne @B ;; note: tests opposite condition for jump
A
goto @return
B
return

&& is like nested ifs
|| is series of ifs where jumps to then-block if condition true

Compare:
_     =  >  < NaN
lcmp  0  1 -1 n/a
dcmpg 0  1 -1   1
dcmpl 0  1 -1  -1
fcmpg 0  1 -1   1
fcmpl 0  1 -1  -1
follow by ifle etc
NaN if either arg is NaN
Java compiles comparison->boolean same as if-else

Method return types: int long float double reference void

JLS:
  An instruction operating on values of type int is also permitted to operate on values of type boolean, byte, char, and short.

  As noted in §2.3.4 and §2.11.1, the Java Virtual Machine internally converts values of types boolean, byte, short, and char to type int.) 

"
#_
(defn if-sf [ctx form]
  (let [test (nth forms 1)
        then (nth forms 2)
        else (nth forms 3)
        success-label (new-label)
        fail-label (new-label)
        end-label (new-label)]
    (-emitConditional ctx test success-label fail-label)
    (-emitLabel ctx success-label)
    (-emitForm ctx then)
    (-emitGoto ctx end-label)
    (-emitLabel ctx fail-label)
    (-emitForm ctx else)
    (-emitLabel ctx end-label)))

#_
(defn ifc-or-sf [ctx form]
  ;; in if-test context
  (let [success-label ?
        fail-label ?]
    (doseq [cf (subvec form 1)]
      (-emitConditional ctx cf success-label fail-label)))
  )

; [[:if-acmpne fail-label]]
#_
(defn ifc-refeq-sf [ctx form]
  (let [success-label ?
        fail-label ?]
    (-emitBranch ctx :ref success-label fail-label)))

(defprotocol PCompilerCtx
  (-emitInsn [_ insn] [_ insn operand])
  (-emitJump [_ insn label])
  (-emitLabel [_ label])
  (-emitLdcInsn [_ value])
  (-emitTypeInsn [_ op type])
  (-emitNewArray [_ type ndims])
  (-emitInvokeMethod [_ op owner interface? method sig])
  (-emitInvokeDynamic [_ name method-type bsmhandle bsmargs])
  (-emitLoadLocal [_ id])
  (-emitStoreLocal [_ sort id])
  (-emitTryCatch [_ start end hander ex-type])
  (-emitFieldInsn [_ op owner field type])
  (-emitLoadSelf [_])
  (-emitTableSwitch [_ min max fallback-label case-labels])
  (-emitLookupSwitch [_ fallback-label keys labels])
  (-emitLineNumber [_ n label])
  
  (-regJumpTarget [_ id label])
  (-unregJumpTarget [_ id])
  (-getJumpTarget [_ id])
  (-selfType [_])
  
  #_(-emitDeclareLocal [_ name]))

(defn emit-int-value [ctx value]
  (if-some [li (case (int value)
                 -1 :iconst-m1 0 :iconst-0 1 :iconst-1 2 :iconst-2
                 3 :iconst-3 4 :iconst-4 5 :iconst-5 nil)]
    (-emitInsn ctx li)
    (cond
      (<= Byte/MIN_VALUE value Byte/MAX_VALUE)
      (-emitInsn ctx :bipush value)
      (<= Short/MIN_VALUE value Short/MAX_VALUE)
      (-emitInsn ctx :sipush value)
      :else
      (-emitLdcInsn ctx (int value)))))

(defn emit-const-prim [ctx {:keys [value]}]
  (let [cls (class value)]
    (if (#{Integer Boolean Byte Character Short} cls)
      (emit-int-value ctx (if (= Boolean cls) (if value 1 0) value))
      (condp = cls
        Long
        (if-some [li (case value
                       0 :lconst-0 1 :lconst-1 nil)]
          (-emitInsn ctx li)
          (-emitLdcInsn ctx value))
        Float
        (if-some [li (case (double value)
                       0. :fconst-0 1. :fconst-1 2. :fconst-2 nil)]
          (-emitInsn ctx li)
          (-emitLdcInsn ctx value))
        Double
        (if-some [li (case value
                       0. :dconst-0 1. :dconst-1 nil)]
          (-emitInsn ctx li)
          (-emitLdcInsn ctx value))))))

(defn emit-nil [ctx _node]
  (-emitInsn ctx :aconst-null))

(defn emit-box [ctx atype]
  (-emitInvokeMethod ctx :static
    atype false "valueOf" [(type/unbox atype) atype]))

(declare emit-ast-node)

(defn emit-discard-return [ctx node]
  (when-not (= "void" (spec/prim? (:node/spec node)))
    (-emitInsn ctx :pop)))

(defn emit-do [ctx {:keys [children]}]
  (when-some [lc (peek children)]
    (run! (fn [c]
            (emit-ast-node ctx c))
      (pop children))
    (emit-ast-node ctx lc)))

(defn emit-assign-local [ctx {:keys [statement? local-name local-id val] :as node}]
  (let [prim (spec/prim? (:node/spec val))
        typ (if prim (keyword prim) :ref)
        id (or local-id (get-in node [:node/locals local-name :id]))]
    (assert (some? id))
    (emit-ast-node ctx val)
    (-emitStoreLocal ctx typ id)
    (when-not statement?
      (-emitLoadLocal ctx id))))

(defn emit-local-use [ctx {:keys [local-name local-id] :as node}]
  (let [id local-id #_(get-in node [:node/locals local-name :id])]
    (assert (some? id) {:local-name local-name
                        :local-id local-id
                        :local (get-in node [:node/locals local-name])
                        :locals (keys (:node/locals node))})
    (-emitLoadLocal ctx id)))

;; TODO get-ret-node to specialise to iinc
;; TODO specialise to neg if (- 0 x)
(defn emit-arithmetic-2 [ctx {:keys [type op arg1 arg2]}]
  (emit-ast-node ctx arg1)
  (emit-ast-node ctx arg2)
  (let [prefix (condp = type :int "i" :long "l" :float "f" :double "d"
                 :byte "i" :short "i" :char "i" :boolean "i")
        opsuf (condp = op :add "add" :subtract "sub" :multiply "mul"
                :divide "div" :and "and" :or "or" :xor "xor" :remainder "rem")]
    (-emitInsn ctx (keyword (str prefix opsuf)))))

(defn emit-arithmetic-1 [ctx {:keys [op arg]}]
  (emit-ast-node ctx arg)
  (let [prim (spec/prim? (:node/spec arg))
        _ (assert (some? prim))
        prim (keyword prim)
        ]
    (case op
      :not
      (let [xor-insn (if (= :long prim) :lxor :ixor)]
        (if (= :long prim)
          (-emitLdcInsn ctx (long -1))
          (-emitInsn ctx :iconst-m1))
        (-emitInsn ctx xor-insn))
      :negate
      (-emitInsn ctx
        (case prim
          :byte :ineg
          :short :ineg
          :int :ineg
          :long :lneg
          :float :fneg
          :double :dneg)))))

(defn num-cmp-insns [op' type']
  ;; TODO investigate non-branching bit arithmetic
  #_
  (match [op' type']
    [op :ref]
    [nil (condp = op := :if-acmpne :not= :if-acmpeq)]
    [op (_ :guard #{:boolean :byte :short :char :int})]
    (condp = op
      := [:ixor :dup :ineg :ior Integer/MIN_VALUE :ixor :iconst-0 :iand]
      :not= [:ixor]
      :>= [:swap :isub Integer/MIN_VALUE :iand :iconst-0 :ixor]
      :> [:isub Integer/MIN_VALUE :iand] ;; ideally [A :ineg B :iadd MIN :iand]
      :<= [:swap :isub Integer/MIN_VALUE :iand :iconst-0 :ixor]
      :< [:isub Integer/MIN_VALUE :iand])
    [op :long]
    (condp = op
      := [:lxor :lconst-0 :lxor] ;; note: will not work
      :not= [:lxor]
      :>= [:swap :lsub Long/MIN_VALUE :land :lconst-0 :lxor]
      :> [:lsub Long/MIN_VALUE :land] ;; ideally [A :lneg B :ladd MIN :land]
      :<= [:swap :lsub Long/MIN_VALUE :land :lconst-0 :lxor]
      :< [:lsub Long/MIN_VALUE :land])
    [op type]
    (let [[cmpl cmpg] (condp = type
                        :double [:dcmpl :dcmpg] :float [:fcmpl :fcmpg])]
      (condp = op
        := [cmpl :ifne]
        :not= [cmpl :ifeq]
        :>= [cmpl :iflt]
        :> [cmpl :ifle]
        :<= [cmpg :ifgt]
        :< [cmpg :ifge])))
  (match [op' type']
    [op :ref]
    [nil (condp = op := :if-acmpne :not= :if-acmpeq)]
    [op (_ :guard #{:boolean :byte :short :char :int})]
    [nil (condp = op
           := :if-icmpne
           :not= :if-icmpeq
           :>= :if-icmplt
           :> :if-icmple
           :<= :if-icmpgt
           :< :if-icmpge)]
    [op :long]
    [:lcmp (condp = op
             := :ifne
             :not= :ifeq
             :>= :iflt
             :> :ifle
             :<= :ifgt
             :< :ifge)]
    [op type]
    (let [[cmpl cmpg] (condp = type
                        :double [:dcmpl :dcmpg] :float [:fcmpl :fcmpg])]
      (condp = op
        := [cmpl :ifne]
        :not= [cmpl :ifeq]
        :>= [cmpl :iflt]
        :> [cmpl :ifle]
        :<= [cmpg :ifgt]
        :< [cmpg :ifge]))))
;; TODO optimise for int cmp with 0
(defn emit-num-cmp-2 [ctx {:keys [type op arg1 arg2]}]
  (let [fail-label (Label.)
        done-label (Label.)
        [cmpi iffi] (num-cmp-insns op type)]
    (emit-ast-node ctx arg1)
    (emit-ast-node ctx arg2)
    (when cmpi (-emitInsn ctx cmpi))
    (-emitJump ctx iffi fail-label)
    (-emitInsn ctx :iconst-1)
    (-emitJump ctx :goto done-label)
    (-emitLabel ctx fail-label)
    (-emitInsn ctx :iconst-0)
    (-emitLabel ctx done-label)))

(defn emit-if-true [ctx {:keys [test then else]}]
  (let [fail-label (Label.)
        done-label (Label.)
        iffi :ifeq]
    (emit-ast-node ctx test)
    (-emitJump ctx iffi fail-label)
    (emit-ast-node ctx then)
    (-emitJump ctx :goto done-label)
    (-emitLabel ctx fail-label)
    (emit-ast-node ctx else)
    (-emitLabel ctx done-label)))

(defn emit-if-nil [ctx {:keys [test then else]}]
  (let [fail-label (Label.)
        done-label (Label.)
        iffi :ifnonnull]
    (emit-ast-node ctx test)
    (-emitJump ctx iffi fail-label)
    (emit-ast-node ctx then)
    (-emitJump ctx :goto done-label)
    (-emitLabel ctx fail-label)
    (emit-ast-node ctx else)
    (-emitLabel ctx done-label)))

(defn emit-instance-of [ctx {:keys [^String classname arg]}]
  (emit-ast-node ctx arg)
  (-emitTypeInsn ctx :instanceof classname))

;; how java determines table/lookupswitch: https://github.com/openjdk/jdk/blob/e7d0ab227ff86bb65abf7fbeb135ce657454200b/src/jdk.compiler/share/classes/com/sun/tools/javac/jvm/Gen.java#L1320
(defn determine-switch-opcode [ncases min-n max-n]
  (let [table-space-cost (+ 4 1 (- max-n min-n))
        table-ncmps 3
        lookup-space-cost (+ 3 (* 2 ncases))
        lookup-ncmps ncases]
    (if (and (< 0 ncases)
          (<=
            (+ table-space-cost (* 3 table-ncmps))
            (+ lookup-space-cost (* 3 lookup-ncmps))))
      Opcodes/TABLESWITCH
      Opcodes/LOOKUPSWITCH)))

(defn emit-case [ctx {:keys [mode classname test keymap cases fallback]}]
  (assert (#{:enum :int} mode))
  (emit-ast-node ctx test)
  (let [case-labels (mapv (fn [_] (Label.)) cases)
        key->label #(nth case-labels (get keymap %))
        fb-label (Label.)
        end-label (Label.)
        ncases (count keymap)
        ;; if enum, sort alphabetically to increase chance of table
        ;; offsets matching ordinals.
        ;; otherwise, sorting of ints also needed for lookupswitch
        case-keys (vec (sort (keys keymap)))
        [min-n max-n] (if (= :enum mode)
                        [1 ncases]
                        [(first case-keys) (peek case-keys)])
        table-switch? (= Opcodes/TABLESWITCH
                        (determine-switch-opcode ncases
                          min-n max-n))]
    (when (= :enum mode)
      (-emitInvokeDynamic ctx "idx"
        (Type/getMethodType Type/INT_TYPE
          (into-array Type [(type/obj-classname->type classname)]))
        (Handle. Opcodes/H_INVOKESTATIC
         "sq/lang/EnumSwitchMapCallSite" "bsm"
          (str "(Ljava/lang/invoke/MethodHandles$Lookup;"
            "Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/String;"
            "[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;")
         false)
        (into [classname] case-keys)))
    (if table-switch?
      (-emitTableSwitch ctx min-n max-n fb-label
        (if (= :enum mode)
          (mapv key->label case-keys)
          (loop-zip [k ^Iterable case-keys]
            [i (first case-keys)
             labels []]
            (let [label (key->label k)]
              (recur
                (inc k)
                (conj (into labels (mapv (constantly fb-label) (range i k)))
                  label)))
            labels)))
      (-emitLookupSwitch ctx fb-label (if (= :enum mode)
                                        (range min-n (inc max-n))
                                        case-keys)
        (mapv key->label case-keys)))
    (doseq [[label c] (map vector case-labels cases)]
      (-emitLabel ctx label)
      (emit-ast-node ctx c)
      (-emitJump ctx :goto end-label))
    (-emitLabel ctx fb-label)
    (if fallback
      (emit-ast-node ctx fallback)
      (do ;; no match error
        (-emitTypeInsn ctx :new "java.lang.Error")
        (-emitInsn ctx :dup)
        (-emitLdcInsn ctx "No match for case")
        (-emitInvokeMethod ctx :special (type/obj-classname->type "java.lang.Error")
          false "<init>"
          (Type/getMethodType Type/VOID_TYPE
            (into-array Type [(Type/getObjectType "java/lang/String")])))
        (-emitInsn ctx :athrow)))
    (-emitLabel ctx end-label)))

(defn emit-throw [ctx {:keys [arg]}]
  (emit-ast-node ctx arg)
  (-emitInsn ctx :athrow))

(defn emit-locking [ctx {:keys [lock body]}]
  (let [sl (Label.) el (Label.) hl (Label.) dl (Label.)
        lock-lid (str (gensym "lock_"))]
    (emit-ast-node ctx lock)
    (-emitInsn ctx :dup)
    (-emitStoreLocal ctx :ref lock-lid)
    (-emitInsn ctx :monitorenter)
    ;; try
    (-emitTryCatch ctx sl el hl nil)
    (-emitLabel ctx sl)
    (emit-ast-node ctx body)
    (-emitLoadLocal ctx lock-lid)
    (-emitInsn ctx :monitorexit)
    (-emitLabel ctx el) ;; exception
    (-emitJump ctx :goto dl)
    (-emitLabel ctx hl)
    (-emitLoadLocal ctx lock-lid)
    (-emitInsn ctx :monitorexit)
    (-emitInsn ctx :athrow)
    (-emitLabel ctx dl)))

(defn emit-try [ctx {:keys [try-body catches finally-body]}]
  (let [start-label (Label.)
        try-end (Label.)
        done-label (Label.)
        [catchall? catches]
        (reduce (fn [[catchall? catches] {:keys [classnames local-id] :as catch}]
                  (let [label (Label.)]
                    (doseq [in (mapv type/classname->type classnames)]
                      (-emitTryCatch ctx start-label try-end label in))
                    [(or catchall? (boolean (some #{"java.lang.Throwable"} classnames)))
                     (conj catches
                       (assoc catch :label label
                         :local-id local-id))]))
          [false []] catches)
        catch-finally-label (Label.)
        uncaught-finally (and (not catchall?) finally-body (Label.))
        emit-finally (fn []
                       (emit-ast-node ctx finally-body)
                       (when-not (= "void" (spec/prim? (:node/spec finally-body)))
                         (-emitInsn ctx :pop)))]
    (when uncaught-finally
      (-emitTryCatch ctx start-label try-end uncaught-finally nil))
    (-emitLabel ctx start-label)
    (emit-ast-node ctx try-body)
    (-emitLabel ctx try-end)
    ;; Normal finally
    (when finally-body
      (emit-finally))
    (-emitJump ctx :goto done-label)
    ;; Uncaught finally
    (when uncaught-finally
      (let [lid (str (gensym "exception_"))]
        (-emitLabel ctx uncaught-finally)
        (-emitStoreLocal ctx :ref lid)
        (emit-finally)
        (-emitLoadLocal ctx lid)
        (-emitInsn ctx :athrow)))
    ;; Catches
    (doseq [{:keys [body label local-id]} catches]
      (-emitLabel ctx label)
      (-emitStoreLocal ctx :ref local-id)
      (emit-ast-node ctx body)
      (-emitJump ctx :goto catch-finally-label))
    ;; Catch -> finally
    (-emitLabel ctx catch-finally-label)
    (when finally-body
      (emit-finally))
    (-emitLabel ctx done-label)))

(defn emit-new-obj [ctx {:keys [classname args method-type node/env]}]
  (let [dynamic? (interop/dynamic-class? env
                   (interop/resolve-class env classname))
        type (type/obj-classname->type classname)]
    (when-not dynamic?
      (-emitTypeInsn ctx :new classname)
      (-emitInsn ctx :dup))
    (doseq [arg args]
      (emit-ast-node ctx arg))
    (if dynamic?
      (-emitInvokeDynamic ctx "(ctor)"
        (Type/getMethodType (Type/getObjectType "java/lang/Object")
          (into-array Type (map (partial as-impl-type env) (.getArgumentTypes method-type))))
        (Handle. Opcodes/H_INVOKESTATIC
          "sq/lang/DynConstructorCallSite" "bsm"
          (str "(Ljava/lang/invoke/MethodHandles$Lookup;"
            "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
            "Ljava/lang/String;"
            ")Ljava/lang/invoke/CallSite;")
          false)
        (object-array [classname]))
      (-emitInvokeMethod ctx :special
        type false "<init>" method-type))))

(defn classname->ary-store-insn [classname]
  (condp = classname
    "int" :iastore
    "long" :lastore
    "double" :dastore
    "float" :fastore
    "byte" :bastore
    "boolean" :bastore
    "char" :castore
    "short" :sastore
    :aastore))

(defn emit-new-array [ctx {:keys [classname ndims dims items]}]
  (assert (vector? dims))
  (doseq [d dims]
    (emit-ast-node ctx d))
  (-emitNewArray ctx (type/classname->type classname) ndims)
  (doseq [[i item] (map-indexed vector items)]
    (when item
      (-emitInsn ctx :dup)
      (emit-int-value ctx i)
      (emit-ast-node ctx item)
      (-emitInsn ctx (classname->ary-store-insn classname)))))

(defn emit-jcall
  [ctx {:keys [classname target interface? spread
               method-name args method-type dynamic node/env]}]
  (let [classname (or classname (spec/get-exact-class (:node/spec target)))
        method-name (interop/munge-method-name method-name)
        ; sig (conj (mapv (comp type/classname->type spec/get-exact-class :node/spec) args)
        ;       (type/classname->type (spec/get-exact-class (:node/spec node))))
        target-class (interop/resolve-class env classname)
        interface? (interop/-interface? target-class)
        method-arg-types (.getArgumentTypes method-type)
        method-type' (Type/getMethodType
                      (as-impl-type env (.getReturnType method-type))
                      (into-array Type (into []
                                         (map (partial as-impl-type env)
                                           method-arg-types))))]
    (when target (emit-ast-node ctx target))
    (doseq [i (range (if spread (dec (count method-arg-types)) (count args)))]
      (let [a (nth args i)]
        (emit-ast-node ctx a)))
    (when spread
      (let [last-type (last method-arg-types)
            vartype (.getElementType last-type)
            nfixed (dec (count method-arg-types))
            ndims (.getDimensions last-type)
            dims (mapv (fn [i] {:node/kind :const-prim :value (int i)})
                   (into [(- (count args) nfixed)] (repeat (dec ndims) 0)))
            items (drop nfixed args)]
        (emit-new-array ctx
         {:classname (type/get-classname vartype)
          :ndims ndims
          :dims dims
          :items items})))
    (when-not dynamic
      (when (interop/dynamic-class? env target-class)
        (throw (ex-info "attempted to link to dynamic class"
                 {:class target-class}))))
    (if dynamic
      (-emitInvokeDynamic ctx method-name
        (if target
          (Type/getMethodType
            (.getReturnType method-type')
            (into-array Type (into [(Type/getObjectType "java/lang/Object")]
                               (.getArgumentTypes method-type'))))
          method-type')
        (Handle. Opcodes/H_INVOKESTATIC
          (if target
            "sq/lang/DynInstanceMethodCallSite"
            "sq/lang/DynClassMethodCallSite")
          "bsm"
          (str "(Ljava/lang/invoke/MethodHandles$Lookup;"
            "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
            "Ljava/lang/String;[Ljava/lang/String;"
            ")Ljava/lang/invoke/CallSite;")
          false)
        (into [classname](map type/get-classname) (.getArgumentTypes method-type)))
      (-emitInvokeMethod ctx (if target (if interface? :interface :virtual) :static)
        (type/obj-classname->type classname) interface? method-name method-type))))

(defn emit-jcall-specific
  [ctx {:keys [classname target method-name args method-type dynamic? node/env]}]
  (let [classname (or classname (spec/get-exact-class (:node/spec target)))
        method-name (interop/munge-method-name method-name)
        ; sig (conj (mapv (comp type/classname->type spec/get-exact-class :node/spec) args)
        ;       (type/classname->type (spec/get-exact-class (:node/spec node))))
        ]
    (emit-ast-node ctx target)
    (doseq [a args]
      (emit-ast-node ctx a))
    (if dynamic?
      (throw (ex-info "noimpl" {}))
      (-emitInvokeMethod ctx :special
        (type/obj-classname->type classname)
        (interop/-interface? (interop/resolve-class env classname))
        method-name method-type))))

(defn emit-get-field
  [ctx {:keys [field-name target classname ^Type field-type dynamic node/env]}]
  (assert (some? field-type))
  (let [self? (= classname (.getClassName ^Type (-selfType ctx)))
        dynamic-field (interop/dynamic-class? env
                        (interop/resolve-class env (type/get-classname field-type)))
        dynamic-target (interop/dynamic-class? env
                        (interop/resolve-class env classname))]
    (when target (emit-ast-node ctx target))
    (if (and (not self?) (or dynamic dynamic-field dynamic-target))
      (-emitInvokeDynamic ctx field-name
        (Type/getMethodType (as-impl-type env field-type)
          (into-array Type [(type/obj-classname->type
                              (static-classname env classname))]))
        (Handle. Opcodes/H_INVOKESTATIC
          (when target "sq/lang/DynGetFieldCallSite")
          "bsm"
          (str "(Ljava/lang/invoke/MethodHandles$Lookup;"
            "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
            "Ljava/lang/String;Ljava/lang/String;"
            ")Ljava/lang/invoke/CallSite;")
          false)
        [classname (type/get-classname field-type)])
      (-emitFieldInsn ctx (if target :getfield :getstatic)
        (type/obj-classname->type classname) field-name
        (as-impl-type env field-type)))))

(defn emit-set-field
  [ctx {:keys [field-name target classname ^Type field-type node/env val statement?]}]
  (assert (some? field-type))
  (let [self? (= classname (.getClassName ^Type (-selfType ctx)))
        dynamic-field (interop/dynamic-class? env
                        (interop/resolve-class env (type/get-classname field-type)))
        dynamic-target (interop/dynamic-class? env
                        (interop/resolve-class env classname))
        field-type' (as-impl-type env field-type)]
    (when target (emit-ast-node ctx target))
    (emit-ast-node ctx val)
    (when-not statement?
      (let [wide? (when (type/prim? field-type)
                    (#{:long :double}
                      (keyword (type/get-classname field-type))))]
        (-emitInsn ctx (if wide? :dup2-x1 :dup-x1))))
    (if (and (not self?) (or dynamic-field dynamic-target))
      (-emitInvokeDynamic ctx field-name
        (Type/getMethodType Type/VOID_TYPE
          (into-array Type [(type/classname->type
                              (static-classname env classname))
                            (as-impl-type env field-type)]))
        (Handle. Opcodes/H_INVOKESTATIC
          (when target "sq/lang/DynSetFieldCallSite")
          "bsm"
          (str "(Ljava/lang/invoke/MethodHandles$Lookup;"
            "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
            "Ljava/lang/String;Ljava/lang/String;"
            ")Ljava/lang/invoke/CallSite;")
          false)
        [classname (type/get-classname field-type)])
      (-emitFieldInsn ctx (if target :putfield :putstatic)
        (type/obj-classname->type classname) field-name field-type'))))

(defn emit-self-get-field [ctx {:keys [field-name node/spec node/env]}]
  (let [class-type (-selfType ctx)
        field-type (type/classname->type (spec/get-exact-class spec))
        ; field-classname (type/get-classname field-type)
        ; dynamic (interop/dynamic-class? env
        ;           (interop/resolve-class env field-classname))
        field-type (as-impl-type env field-type)]
    (-emitLoadSelf ctx)
    (-emitFieldInsn ctx :getfield
      class-type field-name field-type)))

(defn emit-self-set-field [ctx {:keys [field-name ^Type type val node/env statement?]}]
  (let [self-type (-selfType ctx)
        field-classname (type/get-classname type)
        ; dynamic? (interop/dynamic-class? env
        ;            (interop/resolve-class env field-classname))
        type (as-impl-type env type)]
    (if statement?
      (do
        (-emitLoadSelf ctx)
        (emit-ast-node ctx val))
      (let [prim? (type/prim? type)
            wide? (#{:long :double} (when prim? (keyword (type/get-classname type))))]
        (-emitLoadSelf ctx)
        (emit-ast-node ctx val)
        (-emitInsn ctx (if wide? :dup2-x1 :dup-x1))))
    ;; can't use invokedynamic for field set - may be final and in ctor
    (-emitFieldInsn ctx :putfield self-type field-name type)))

(defn emit-jcall-super [ctx {:keys [classname method-type args]}]
  (-emitLoadSelf ctx)
  (doseq [a args]
    (emit-ast-node ctx a))
  (-emitInvokeMethod ctx :special (type/obj-classname->type classname)
    false "<init>" method-type))

(defn emit-string [ctx {:keys [value]}]
  (-emitLdcInsn ctx value))

(defn emit-char [ctx {:keys [value]}]
  (-emitLdcInsn ctx value))

(defn emit-restart-target [ctx {:keys [id body]}]
  (let [label (Label.)]
    (-regJumpTarget ctx id label)
    (-emitLabel ctx label)
    (emit-ast-node ctx body)
    (-unregJumpTarget ctx id)))

(defn emit-jump [ctx {:keys [id]}]
  (-emitJump ctx :goto (-getJumpTarget ctx id)))

(defn emit-cast [ctx {:keys [^Type type body node/env] :as node}]
  (emit-ast-node ctx body)
  (if (type/prim? type)
    (let [from-prim' (:from-prim node)
          from-prim (case from-prim' (:byte :char :short :boolean) :int from-prim')
          to-prim (keyword (type/get-classname type))]
      (when-not (or (nil? from-prim') (= from-prim' to-prim))
        (let [insn (match [from-prim to-prim]
                     [:int :byte] :i2b
                     [:int :char] :i2c
                     [:int :short] :i2s
                     [:long :byte] [:l2i :i2b]
                     [:long :char] [:l2i :i2c]
                     [:long :short] [:l2i :i2s]
                     [:long :int] :l2i
                     [:float :byte] [:f2i :i2b]
                     [:float :char] [:f2i :i2c]
                     [:float :short] [:f2i :i2s]
                     [:float :int] :f2i
                     [:float :long] :f2l
                     [:double :byte] [:d2i :i2b]
                     [:double :char] [:d2i :i2c]
                     [:double :short] [:d2i :i2s]
                     [:double :int] :d2i
                     [:double :float] :d2f
                     [:double :long] :d2l
                     
                     ;; widening
                     [:int :long] :i2l
                     [:int :float] :i2f
                     [:int :double] :i2d
                     [:long :float] :l2f
                     [:long :double] :l2d
                     [:float :double] :f2d
                     )]
          (if (vector? insn)
            (do (-emitInsn ctx (nth insn 0))
              (-emitInsn ctx (nth insn 1)))
            (-emitInsn ctx insn)))))
    (when-not (interop/dynamic-class? env (interop/resolve-class env (type/get-classname type)))
      (-emitTypeInsn ctx :checkcast (.getInternalName type)))))

(defn emit-array-set [ctx {:keys [target index val]}]
  (let [aryt (type/classname->type
               (spec/get-exact-class
                 (spec/get-array-element (:node/spec target))))
        insn (classname->ary-store-insn (.getClassName aryt))]
    (emit-ast-node ctx target)
    (emit-ast-node ctx index)
    (emit-ast-node ctx val)
    (-emitInsn ctx insn)))

(defn emit-array-get [ctx {:keys [target index] :as node}]
  (let [aryt (type/classname->type
               (spec/get-exact-class (:node/spec node)))
        insn (condp = (.getClassName aryt)
               "int" :iaload
               "long" :laload
               "double" :daload
               "float" :faload
               "byte" :baload
               "boolean" :baload
               "char" :caload
               "short" :saload
               :aaload)]
    (emit-ast-node ctx target)
    (emit-ast-node ctx index)
    (-emitInsn ctx insn)))

(defn emit-array-length [ctx {:keys [target node/env]}]
  (emit-ast-node ctx target)
  (when (interop/dynamic-class? env
          (interop/resolve-class env (spec/get-exact-class (:node/spec target))))
    (-emitTypeInsn ctx :checkcast "[Ljava/lang/Object;"))
  (-emitInsn ctx :arraylength))

(defn emit-keyword [ctx {:keys [string]}]
  (-emitLdcInsn ctx
    (ConstantDynamic. (str ":" string)
      "Lsq/lang/Keyword;"
      (Handle. Opcodes/H_INVOKESTATIC
        "java/lang/invoke/ConstantBootstraps"
        "invoke"
        (str "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/Class;"
          "Ljava/lang/invoke/MethodHandle;[Ljava/lang/Object;)Ljava/lang/Object;")
        false)
      (object-array
        [(Handle. Opcodes/H_INVOKESTATIC
           "sq/lang/KeywordFactory" "from"
           "(Ljava/lang/String;)Lsq/lang/Keyword;"
           false)
         string]))))

(defn type-sort->load-op [sort]
  (op/kw->opcode
    (enc/case-eval sort
      (list Type/OBJECT Type/ARRAY) :aload Type/LONG :lload Type/INT :iload Type/FLOAT :fload
      Type/DOUBLE :dload Type/BOOLEAN :iload Type/BYTE :iload
      Type/CHAR :iload Type/SHORT :iload
      (throw (ex-info "invalid sort" {:sort sort})))))

(defn type-sort->store-op [sort]
  (op/kw->opcode
    (enc/case-eval sort
      (list Type/OBJECT Type/ARRAY) :astore Type/LONG :lstore Type/INT :istore Type/FLOAT :fstore
      Type/DOUBLE :dstore Type/BOOLEAN :istore Type/BYTE :istore
      Type/CHAR :istore Type/SHORT :istore
      (throw (ex-info "invalid sort" {:sort sort})))))

(defn type-sort->return-op [sort]
  (op/kw->opcode
    (enc/case-eval sort
      (list Type/OBJECT Type/ARRAY)  :areturn Type/LONG :lreturn Type/INT :ireturn Type/FLOAT :freturn
      Type/DOUBLE :dreturn Type/BOOLEAN :ireturn Type/BYTE :ireturn
      Type/CHAR :ireturn Type/SHORT :ireturn Type/VOID :return
      (throw (RuntimeException. (str sort " not valid sort"))))))

(deftype+ MethodCompilerCtx
  [^MethodVisitor mv
   ^Type self-type
   ^java.util.HashMap locals
   #_^java.util.ArrayList slots
   ^java.util.TreeSet free-slots
   ^:mut nslots
   ^java.util.HashMap jump-targets]
  PCompilerCtx
  (-emitInsn [_ insn]
    (.visitInsn mv (op/kw->opcode-0 insn)))
  (-emitInsn [_ insn operand]
    (.visitIntInsn mv (condp = insn
                     :bipush Opcodes/BIPUSH
                     :sipush Opcodes/SIPUSH
                     :newarray Opcodes/NEWARRAY)
      operand))
  (-emitTypeInsn [_ insn type]
    (.visitTypeInsn mv (op/kw->opcode insn) (.replace ^String type \. \/)))
  (-emitNewArray [_ type ndims]
    (if (= 1 ndims)
      (if (type/prim? type)
        (.visitIntInsn mv Opcodes/NEWARRAY (op/type->opcode type))
        (.visitTypeInsn mv Opcodes/ANEWARRAY (.getInternalName ^Type type)))
      (.visitMultiANewArrayInsn mv (.getDescriptor (type/array-type type ndims)) ndims)))
  (-emitJump [_ insn label]
    (.visitJumpInsn mv (op/kw->opcode insn) label))
  (-emitLabel [_ label]
    (.visitLabel mv label))
  (-emitLdcInsn [_ value]
    (.visitLdcInsn mv value))
  (-emitLoadLocal [_ id]
    (let [l (.get locals id)
          _ (when (nil? l)
              (throw (ex-info (str "Can't load local id: " id) {:locals locals})))
          ts (:type-sort l)
          _ (assert (some? ts))
          insn (type-sort->load-op ts)]
      (.visitVarInsn mv insn (:idx l))))
  (-emitStoreLocal [_ tsort id]
    (let [wide? (contains? #{:long :double} tsort)
          width (if wide? 2 1)
          sort (or (some-> tsort name
                       (type/prim-classname->type)
                       (.getSort))
                   Type/OBJECT)
          insn (type-sort->store-op sort)
          local (.get locals id)
          ;; if different type, new local (shadowing)
          local (when (= sort (:type-sort local)) local)
          idx (or (:idx local)
                (let [n nslots]
                  (set! nslots (+ nslots width))
                  n))]
      (when (nil? local)
        (.put locals id {:idx idx :type-sort sort}))
      (.visitVarInsn mv insn idx)))
  (-emitLoadSelf [_]
    (.visitVarInsn mv Opcodes/ALOAD 0))
  (-emitTryCatch [_ start end handler ex-type]
    ;; first save stack to locals TODO
    (.visitTryCatchBlock mv start end handler
      (some-> ex-type .getInternalName)))
  (-emitInvokeMethod [_ op owner interface? method sig]
    (.visitMethodInsn mv (condp = op
                           :virtual Opcodes/INVOKEVIRTUAL
                           :interface Opcodes/INVOKEINTERFACE
                           :static Opcodes/INVOKESTATIC
                           :special Opcodes/INVOKESPECIAL)
      (.getInternalName ^Type owner)
      method
      (if (instance? Type sig)
        (.getDescriptor ^Type sig)
        (if (string? sig)
          sig
          (Type/getMethodDescriptor (peek sig) (into-array Type (pop sig)))))
      interface?))
  (-emitInvokeDynamic [_ name method-type bsmhandle bsmargs]
    (.visitInvokeDynamicInsn mv name (.getDescriptor ^Type method-type)
      bsmhandle (object-array bsmargs)))
  (-emitFieldInsn [_ op owner-type field type]
    (.visitFieldInsn mv (op/kw->opcode op) (.getInternalName ^Type owner-type)
      field (.getDescriptor ^Type type)))
  (-emitTableSwitch [_ imin imax fb-label case-labels]
    (.visitTableSwitchInsn mv imin imax fb-label (into-array Label case-labels)))
  (-emitLookupSwitch [_ fb-label keys case-labels]
    (.visitLookupSwitchInsn mv fb-label (int-array keys) (into-array Label case-labels)))
  (-emitLineNumber [_ n label]
    (.visitLineNumber mv n label))
  
  (-selfType [_] self-type)
  (-regJumpTarget [_ id label]
    (.put jump-targets id label))
  (-unregJumpTarget [_ id]
    (.remove jump-targets id))
  (-getJumpTarget [_ id]
    (or (.get jump-targets id)
      (throw (RuntimeException. (str "Could not find jump target " id))))))

(defn new-mcctx [mv {:keys [param-types param-ids instance? class-type]}]
  (assert (some? class-type))
  (assert (if instance?
            (= (count param-types) (dec (count param-ids)))
            (= (count param-types) (count param-ids)))
    [(vec param-types) (vec param-ids)])
  (let [local-map (java.util.HashMap.)
        _ (when instance?
            (.put local-map (first param-ids) {:type-sort Type/OBJECT :idx 0}))
        [nslots _]
        (reduce (fn [[nslots idx] [name p]]
                  (let [sort (.getSort p)
                        nslots' (+ nslots
                                  (if (or (= Type/LONG sort)
                                        (= Type/DOUBLE sort))
                                    2 1))]
                    (.put local-map name {:type-sort sort :idx nslots})
                    [nslots' nil]))
          (if instance? [1 1] [0 0])
          (map vector (if instance? (drop 1 param-ids) param-ids) param-types))]
    (->MethodCompilerCtx mv class-type
      local-map
      (java.util.TreeSet.) nslots
      (java.util.HashMap.))))

(defn emit-coercion [ctx coercion]
  (match coercion
    [:insn op]
    (when-not (= op :nop)
      (-emitInsn ctx op))
    [:method side ciname method-name desc]
    (-emitInvokeMethod ctx (case side :instance :virtual :class :static)
      (Type/getObjectType ciname) false
      method-name desc)
    [:do & cos]
    (doseq [co cos]
      (emit-coercion ctx co))))

(defn emit-ast-node [ctx node]
  (when-some [line (:line (:node/source node))]
    (let [l (Label.)]
      (-emitLabel ctx l)
      (-emitLineNumber ctx line l)))
  (let [f(case (:node/kind node)
           :const-prim emit-const-prim
           :nil emit-nil
           :void (constantly nil)
           :do emit-do
           :local-use emit-local-use
           :assign-local emit-assign-local
           :arithmetic-2 emit-arithmetic-2
           :num-cmp-2 emit-num-cmp-2
           :if-true emit-if-true
           :if-nil emit-if-nil
           :new-obj emit-new-obj
           :new-array emit-new-array
           :throw emit-throw
           :locking emit-locking
           :jcall emit-jcall
           :string emit-string
           :char emit-char
           :restart-target emit-restart-target
           :jump emit-jump
           :cast emit-cast
           :try emit-try
           :get-field emit-get-field
           :array-get emit-array-get
           :array-set emit-array-set
           :self-field-get emit-self-get-field
           :self-set-field emit-self-set-field
           :set-field emit-set-field
           :arithmetic-1 emit-arithmetic-1
           :keyword emit-keyword
           :jcall-super emit-jcall-super
           :case emit-case
           :array-length emit-array-length
           :instance-of emit-instance-of
           :jcall-specific emit-jcall-specific
           (throw (ex-info
                    "No handler for bytecode node"
                    {:node (when node
                             (if (map? node)
                               (assoc (select-keys node [:node/kind])
                                 :keys (keys node))
                               node))
                     :type (class node)})))]
    (try
      (f ctx node)
      (if-some [coercion (:node/coercion node)]
        (emit-coercion ctx coercion)
        (when (:node/discard-result node) ;; TODO can represent as coercion
          (let [wide? (#{"long" "double"} (spec/get-exact-class (:node/spec node)))]
            (-emitInsn ctx (if wide? :pop2 :pop)))))
      (catch Throwable e
        (throw (ex-info "Failed to emit node"
                 {:node (select-keys node [:node/kind :node/source])}
                 e))))))

(defn visit-positional-ctor
  [env ^ClassVisitor cv super-iname ^Type class-type fields self-bindname body]
  (let [method-desc (Type/getMethodDescriptor Type/VOID_TYPE
               (into-array Type (mapv (comp (partial as-impl-type env) :type) fields)))
        mv (.visitMethod cv Opcodes/ACC_PUBLIC "<init>"
             method-desc nil nil)
        emit-field-puts
        (fn []
          (doseq [[i {:keys [name type]}]
                  (map-indexed vector fields)]
            (let [type (as-impl-type env type)]
              (.visitVarInsn mv Opcodes/ALOAD 0)
              (.visitVarInsn mv (type-sort->load-op (.getSort type)) (inc i))
              (.visitFieldInsn mv Opcodes/PUTFIELD (.getInternalName class-type)
                name (.getDescriptor type)))))]
    (if body
      (let [ctx (new-mcctx mv
                  {:class-type class-type :instance? true
                   :param-ids (into [self-bindname] (map :name) fields)
                   :param-types (mapv :type fields)})]
        (emit-ast-node ctx body)
        (emit-discard-return ctx body)
        (emit-field-puts)
        (.visitInsn mv Opcodes/RETURN)
        (.visitMaxs mv -1 -1))
      (do
        ;; Emits same bytecode as Java would
        (.visitVarInsn mv Opcodes/ALOAD 0)
        ;; FIXME this assumes there exists a super ctor with 0 args
        (.visitMethodInsn mv Opcodes/INVOKESPECIAL super-iname "<init>" "()V")
        (emit-field-puts)
        (.visitInsn mv Opcodes/RETURN)
        (.visitMaxs mv 1 (inc (count fields)))))
    (.visitEnd mv)))

(defn cl-define-class [classloader classname ba]
  (if (instance? clojure.lang.DynamicClassLoader classloader)
    (.defineClass classloader classname
      ba nil)
    (.defineClassFromBytes classloader classname ba)))

(def *stub-classloader-ctor (atom #(clojure.lang.RT/makeClassLoader)))

;; ideally override getCommonClass, for now create stub for reflection
(defn create-stub-class [{:keys [interfaces flags super classname]}]
  (let [ciname (.replace classname \. \/)
        super-in (if super (.replace super \. \/) "java/lang/Object")
        cv-stub (doto (ClassWriter. 0)
                  (.visit Opcodes/V19 (reduce bit-or 0 (map op/kw->acc-opcode flags))
                    ciname nil super-in
                    (when (seq interfaces)
                      (into-array String (map #(.replace % \. \/) interfaces)))))
        cl (@*stub-classloader-ctor)]
    (cl-define-class cl classname (.toByteArray cv-stub))
    (Class/forName classname true cl)))

(defn classinfo->bytes
  [env {:keys [^String classname ^String super instance-methods
               class-methods interfaces flags class-fields constructors] :as classinfo}]
  (assert (string? classname) classinfo)
  (let [ciname (.replace classname \. \/)
        class-type (Type/getObjectType ciname)
        super-in (if super (.replace super \. \/) "java/lang/Object")  
        cv (doto (proxy [ClassWriter] [ClassWriter/COMPUTE_FRAMES])
             (.visit Opcodes/V19 (reduce bit-or Opcodes/ACC_PUBLIC
                                   (map op/kw->acc-opcode flags)) ciname
               nil super-in
               (when (seq interfaces)
                 (into-array String (map #(.replace % \. \/) interfaces)))))
        _ (.visitSource cv "eval" nil)
        conform-field #(assoc % :type (type/classname->type (:classname %)))  
        instance-fields (mapv conform-field (:fields classinfo)) 
        class-fields (mapv (comp #(update % :flags conj :static) conform-field) class-fields)
        field-inits
        (reduce (fn [acc {:keys [name type flags] :as f}]
                  (let [_fv (.visitField cv (reduce bit-or 0
                                              (map op/kw->acc-opcode flags)) name
                              (.getDescriptor type)
                              nil nil)]
                    ;; TODO if const, set field initialiser instead
                    (conj acc f)))
          [] class-fields)]
    ;; INSTANCE FIELDS
    (doseq [{:keys [name type flags dynamic]} instance-fields]
      (let [_fv (.visitField cv (reduce bit-or 0
                                  (map op/kw->acc-opcode flags)) name
                 (.getDescriptor (as-impl-type env type))
                 nil nil)]))
    ;; METHODS
    (doseq [{:keys [name body ret-classname param-ids param-classnames
                    flags]} (into instance-methods (map #(update % :flags conj :static))
                              class-methods)]
      (let [param-types (into-array Type
                          (mapv (comp type/classname->type (partial static-classname env))
                            param-classnames))
            ret-type (type/classname->type (static-classname env ret-classname))
            mv (.visitMethod cv (reduce bit-or 0
                                  (map op/kw->acc-opcode flags))
                 (interop/munge-method-name name)
                 (Type/getMethodDescriptor ret-type param-types)
                 nil (into-array String ["java/lang/Exception"]))]
        (when body
          (let [ctx (new-mcctx mv
                      {:class-type class-type
                       :instance? (not (contains? flags :static))
                       :param-ids param-ids
                       :param-types param-types})]
            (emit-ast-node ctx body)
            (.visitInsn mv (type-sort->return-op (.getSort ret-type)))
            (try (.visitMaxs mv -1 -1)
              (catch Exception e
                (throw (ex-info (str "Could not computer maxs for: " name)
                         {:bytes (.toByteArray cv)} e))))
            (.visitEnd mv)))))
    ;; CLINIT
    (when (seq field-inits)
      (let [mv (.visitMethod cv Opcodes/ACC_STATIC "<clinit>"
                (Type/getMethodDescriptor Type/VOID_TYPE (make-array Type 0))
                nil (into-array String ["java/lang/Exception"]))
           ctx (new-mcctx mv
                 {:class-type class-type :instance? false
                  :param-ids [] :param-types []})]
       (doseq [{:keys [val name type]} field-inits]
         (emit-ast-node ctx val)
         (-emitFieldInsn ctx :putstatic (Type/getObjectType ciname) name type))
       (.visitInsn mv Opcodes/RETURN)
       (.visitMaxs mv -1 -1)
       (.visitEnd mv)))
    ;; CONSTRUCTOR
    (doseq [{:keys [param-classnames param-ids auto body flags] :as ctor} constructors]
      (if auto ;; TODO ctor
        (visit-positional-ctor env cv super-in class-type instance-fields
          (first param-classnames) body)
        (let [param-types (mapv (comp type/classname->type
                                  (partial static-classname env))
                            param-classnames)
              mv (.visitMethod cv (reduce bit-or 0
                                    (map op/kw->acc-opcode flags)) "<init>"
                   (Type/getMethodDescriptor Type/VOID_TYPE
                     (into-array Type param-types))
                   nil nil)
              ctx (new-mcctx mv
                    {:class-type class-type :instance? true
                     :param-ids param-ids
                     :param-types param-types})]
          (when (= super-in "java/lang/Object")
            (.visitVarInsn mv Opcodes/ALOAD 0)
            (.visitMethodInsn mv Opcodes/INVOKESPECIAL super-in "<init>" "()V"))
          (emit-ast-node ctx body)
          (emit-discard-return ctx body)
          (.visitInsn mv Opcodes/RETURN)
          (try (.visitMaxs mv -1 -1)
            (catch Exception e
              (throw (ex-info (str "Could not computer maxs for ctor")
                       {:bytes (.toByteArray cv)} e))))
          (.visitEnd mv))))
    [classname (.toByteArray cv)]))

(def ^:dynamic *write-bytes* false)

(defn load-ast-classes-hidden
  [{:keys [^MethodHandles$Lookup lookup]} {:keys [node/env] :as node}]
  (let [new-classes (:new-classes env)
        writeout
        (fn [fn ba]
          (java.nio.file.Files/write
            (java.nio.file.Path/of (str "tmp/" (.replace (.replace fn \, \.)
                                                 "_." "")
                                     ".class") (make-array String 0))
            ba
            (make-array java.nio.file.OpenOption 0)))
        new-compiled-classes
        (mapv (fn [info]
                (try 
                  (classinfo->bytes env info)
                  (catch Exception e
                    (when-some [ba (:bytes (ex-data e))]
                      (writeout (:classname info) ba))
                    (throw e))))
          (vals new-classes))]
    (into {}
      (map (fn [[name bytes]]
             (let [lk lookup]
               (writeout name bytes)
               [name
                (try (.defineHiddenClass lk bytes false;true
                      (make-array MethodHandles$Lookup$ClassOption 0))
                 (catch Throwable e
                   (throw (ex-info (str "Could not load hidden class " name)
                            {} e))))])))
      new-compiled-classes)))

(defn ir->eval-bytes
  [{:keys[classname]}node]
  (let [classname (or classname
                    (str "jl.run" (gensym ".Eval_")))
        clsiname (.replace classname \. \/)
        cv (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
             (.visit Opcodes/V19 Opcodes/ACC_PUBLIC clsiname
               nil "java/lang/Object" nil))
        mv (.visitMethod cv (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
             "run" "()Ljava/lang/Object;" nil
             (into-array String ["java/lang/Exception"]))
        sl (Label.)
        el (Label.)
        sl2 (Label.)
        el2 (Label.)
        conform-return
        (fn []
          (let [prim (spec/prim? (:node/spec node))
                ctx (new-mcctx mv {:class-type (Type/getObjectType clsiname)})]
            (emit-ast-node ctx node)
            (when prim
              (if (= "void" prim)
                (-emitInsn ctx :aconst-null)
                (emit-box ctx (type/box (type/prim-classname->type prim)))))))]
    (doto mv
      (.visitCode)
      (do (conform-return))
      (.visitInsn Opcodes/ARETURN))
    (try (.visitMaxs mv -1 -1)
      (.visitEnd mv)
      (.toByteArray cv)
      (catch Exception e
        (throw (ex-info "Failed to compute stack frames" {:bytes (.toByteArray cv)} e))))))

(defn eval-ast
  ([node] (eval-ast {} node))
  ([{:keys [^ClassLoader classloader lookup package-name ^String classname]} node]
   (let [classloader (if (and (nil? classloader) (nil? lookup))
                       (clojure.lang.RT/makeClassLoader)
                       classloader)
         classname (or classname
                     (str (if classloader
                            (or package-name "jl.run")
                            (.getPackageName (.lookupClass lookup)))
                       ".Eval" #_(gensym ".Eval_")))
         writeout
         (fn [fn ba]
           (java.nio.file.Files/write
             (java.nio.file.Path/of (str "tmp/" fn ".class") (make-array String 0))
             ba
             (make-array java.nio.file.OpenOption 0)))
         eval-ba (try (ir->eval-bytes
                        {:classname classname}
                        node)
                   (catch Exception e
                     (when-some [ba (:bytes (ex-data e))]
                       (writeout "eval" ba))
                     (throw (ex-cause e))))
         _ (when *write-bytes*
             (writeout "eval" eval-ba))
         eval-class
         (try (if classloader
                (do
                  #_(.defineClass (or lookup (MethodHandles/lookup)) eval-ba)
                  (cl-define-class classloader classname eval-ba)
                  (Class/forName classname true classloader))
                (.lookupClass
                  (.defineHiddenClass lookup eval-ba
                    true (make-array MethodHandles$Lookup$ClassOption 0))))
           (catch java.lang.ClassFormatError e
             (writeout "eval" eval-ba) (throw e))
           (catch java.lang.VerifyError e
             (writeout "eval" eval-ba) (throw e)))
         new-classes (:new-classes (:node/env node))
         new-compiled-classes (mapv (partial classinfo->bytes (:node/env node))
                                (vals new-classes))]
     
     (doseq [[name bytes] new-compiled-classes]
       (let [cl (or classloader
                  (clojure.lang.DynamicClassLoader.
                    (ClassLoader/getSystemClassLoader)))]
         (when (or (not (.startsWith name "_")) *write-bytes*)
           (writeout name bytes))
         (cl-define-class cl name bytes)
         ;; Force initialise the class with the defining loader
         (Class/forName name true cl)))
     
     (let [p (promise)
           th (Thread/ofPlatform)]
       (.start th
         (fn [] (try (deliver p
                       [:ok #_(eval '(jl.run.Eval/run))
                        (.invoke (first (.getMethods eval-class))
                          eval-class (object-array 0))])
                  (catch Throwable ex (deliver p [:error ex])))))
       (let [rs (deref p 2000 [:timeout nil])]
         (match rs
           [:ok r] r
           [:error e'] (do (writeout "eval" eval-ba) (throw e'))
           [:timeout _] (do (.stop th) ::timeout)))))))

(comment

   
  ;; set! should do same as l= if in same branch as local is declared
  ;; prevent infinite loops: ensure each branch has a path to termination

  )

"
J0 I0 -> 2
J0 I1 -> 2
I0 J1 -> 3
I0 J2 -> 4
Implies local idx refers to slot idx; confirmed in source

Can store & eval int/null in long var -> suggests local descriptor does nothing.
Descriptor used by COMPUTE_MAXS for slot size, and for LocalVariableTable which is optional
for the benefit of debuggers. signature -> LocalVariableTypeTable, for debuggers.

Can not call visitLocalVariable at all, and still works fine, and no table.

"









