(ns jl.compiler.core
  (:require
    [clojure.core.match :refer [match]]
    [chic.util :refer [deftype+]]
    [jl.compiler.type :as type]
    [jl.compiler.op :as op]
    [jl.compiler.spec :as spec])
  (:import
    (org.objectweb.asm ClassVisitor MethodVisitor Opcodes ClassWriter Type Label)))

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
  (-emitInvokeMethod [_ op owner method sig])
  (-emitLoadLocal [_ id])
  (-emitStoreLocal [_ sort id])
  (-emitTryCatch [_ start end hander ex-type])
  (-emitFieldInsn [_ op owner field type])
  
  (-regJumpTarget [_ id label])
  (-unregJumpTarget [_ id])
  (-getJumpTarget [_ id])
  
  #_(-emitDeclareLocal [_ name]))

(defn emit-const-prim [ctx {:keys [value]}]
  (let [cls (type value)]
    (if (#{Integer Boolean Byte Character Short} cls)
      (if-some [li (case (int (if (= Boolean cls) (if value 1 0) value))
                     -1 :iconst-m1 0 :iconst-0 1 :iconst-1 2 :iconst-2
                     3 :iconst-3 4 :iconst-4 5 :iconst-5 nil)]
        (-emitInsn ctx li)
        (cond
          (<= Byte/MIN_VALUE value Byte/MAX_VALUE)
          (-emitInsn ctx :bipush value)
          (<= Short/MIN_VALUE value Short/MAX_VALUE)
          (-emitInsn ctx :sipush value)
          :else
          (-emitLdcInsn ctx (int value))))
      (condp = cls
        Long
        (if-some [li (case value
                       0 :lconst-0 1 :lconst-1 nil)]
          (-emitInsn ctx li)
          (-emitLdcInsn ctx value))))))

(defn emit-nil [ctx _node]
  (-emitInsn ctx :aconst-null))

(defn emit-box [ctx atype]
  (-emitInvokeMethod ctx :static
    atype "valueOf" [(type/unbox atype) atype]))

(declare emit-ast-node)

(defn emit-do [ctx {:keys [children]}]
  (when-some [lc (peek children)]
    (run! (fn [c]
            (emit-ast-node ctx c)
            (when-not (= "void" (spec/prim? (:node/spec c)))
              (-emitInsn ctx :pop)))
      (pop children))
    (emit-ast-node ctx lc)))

(defn emit-assign-local [ctx node]
  (let [val (:val node)
        prim (spec/prim? (:node/spec val))
        typ (if prim (keyword prim) :ref)]
    (emit-ast-node ctx val)
    ; (-emitInsn ctx :dup)
    (-emitStoreLocal ctx typ (:local-name node))))

(defn emit-local-use [ctx {:keys [local-name]}]
  (-emitLoadLocal ctx local-name))

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

(defn num-cmp-insns [op' type']
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

(defn emit-new-obj [ctx {:keys [classname args]}]
  (doseq [arg args]
    (emit-ast-node ctx arg))
  (-emitTypeInsn ctx :new classname)
  (let [type (type/obj-classname->type classname)]
    (-emitInsn ctx :dup)
    (-emitInvokeMethod ctx :special type "<init>" [Type/VOID_TYPE])))

(defn emit-new-array [ctx {:keys [classname ndims dims]}]
  (doseq [d dims]
    (emit-ast-node ctx d))
  (-emitNewArray ctx (type/classname->type classname) ndims))

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
        (reduce (fn [[catchall? catches] {:keys [classnames local-name] :as catch}]
                  (let [label (Label.)]
                    (doseq [in (mapv type/classname->type classnames)]
                      (-emitTryCatch ctx start-label try-end label in))
                    [(or catchall? (boolean (some #{"java.lang.Throwable"} classnames)))
                     (conj catches
                       (assoc catch :label label
                         :local-id local-name))]))
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

(defn emit-jcall [ctx {:keys [classname target method-name args method-type] :as node}]
  (let [classname (or classname (spec/get-exact-class (:node/spec target)))
        ; sig (conj (mapv (comp type/classname->type spec/get-exact-class :node/spec) args)
        ;       (type/classname->type (spec/get-exact-class (:node/spec node))))
        ]
    (when target (emit-ast-node ctx target))
    (doseq [a args]
      (emit-ast-node ctx a))
    (-emitInvokeMethod ctx (if target :virtual :static)
      (type/obj-classname->type classname) method-name method-type)))

(defn emit-get-field [ctx {:keys [field-name target classname type]}]
  (when target (emit-ast-node ctx target))
  (-emitFieldInsn ctx (if target :getfield :getstatic)
    (type/obj-classname->type classname) field-name type))

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

(defn emit-cast [ctx {:keys [^Type type body] :as node}]
  (emit-ast-node ctx body)
  (if (type/prim? type)
    (let [from-prim' (:from-prim node)
          from-prim (case from-prim' (:byte :char :short :boolean) :int from-prim')
          to-prim (keyword (.getClassName type))]
      (when-not (= from-prim' to-prim)
        (let [insn (match [from-prim to-prim]
                  [:int :long] :i2l
                  [:int :float] :i2f
                  [:int :double] :i2d
                  [:int :byte] :i2b
                  [:int :char] :i2c
                  [:int :short] :i2s
                  [:long :int] :l2i
                  [:long :float] :l2f
                  [:long :double] :l2d
                  [:long :byte] [:l2i :i2b]
                  [:long :char] [:l2i :i2c]
                  [:long :short] [:l2i :i2s]
                  [:float :int] :f2i
                  [:float :long] :f2l
                  [:float :double] :f2d
                  [:float :byte] [:f2i :i2b]
                  [:float :char] [:f2i :i2c]
                  [:float :short] [:f2i :i2s]
                  [:double :int] :d2i
                  [:double :long] :d2l
                  [:double :float] :d2f
                  [:double :byte] [:d2i :i2b]
                  [:double :char] [:d2i :i2c]
                  [:double :short] [:d2i :i2s])]
          (if (vector? insn)
            (do (-emitInsn ctx (nth insn 0))
              (-emitInsn ctx (nth insn 1)))
            (-emitInsn ctx insn)))))
    (-emitTypeInsn ctx :checkcast (.getInternalName type))))

(defn emit-array-set [ctx {:keys [target index val]}]
  (let [aryt (type/classname->type
               (spec/get-exact-class
                 (spec/get-array-element (:node/spec target))))
        insn (condp = (.getClassName aryt)
               "int" :iastore
               "long" :lastore
               "double" :dastore
               "float" :fastore
               "byte" :bastore
               "boolean" :bastore
               "char" :castore
               "short" :sastore
               :aastore)]
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

(deftype+ MethodCompilerCtx
  [^MethodVisitor mv ^java.util.HashMap locals
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
          insn (condp = (:type-sort l)
                 :ref :aload :long :lload :int :iload :float :fload
                 :double :dload :boolean :iload :byte :iload :char :iload :short :iload)]
      (.visitVarInsn mv (op/kw->opcode insn) (:idx l))))
  (-emitStoreLocal [_ tsort id]
    (let [wide? (contains? #{:long :double} tsort)
          width (if wide? 2 1)
          insn (condp = tsort
                 :ref :astore :long :lstore :int :istore :float :fstore
                 :double :dstore :boolean :istore :byte :istore :char :istore :short :istore)
          local (.get locals id)
          idx (or (:idx local)
                (let [n nslots]
                  (set! nslots (+ nslots width))
                  n))]
      (when (nil? local)
        (.put locals id {:idx idx :type-sort tsort}))
      (.visitVarInsn mv (op/kw->opcode insn) idx)))
  (-emitTryCatch [_ start end handler ex-type]
    ;; first save stack to locals TODO
    (.visitTryCatchBlock mv start end handler
      (some-> ex-type .getInternalName)))
  (-emitInvokeMethod [_ op owner method sig]
    (.visitMethodInsn mv (condp = op
                           :virtual Opcodes/INVOKEVIRTUAL
                           :interface Opcodes/INVOKEINTERFACE
                           :static Opcodes/INVOKESTATIC
                           :special Opcodes/INVOKESPECIAL)
      (.getInternalName ^Type owner) method
      (if (instance? Type sig)
        (.getDescriptor ^Type sig)
        (Type/getMethodDescriptor (peek sig) (into-array Type (pop sig))))))
  (-emitFieldInsn [_ op owner field type]
    (.visitFieldInsn mv (op/kw->opcode op) (.getInternalName ^Type owner)
      field (.getDescriptor ^Type type)))
  (-regJumpTarget [_ id label]
    (.put jump-targets id label))
  (-unregJumpTarget [_ id]
    (.remove jump-targets id))
  (-getJumpTarget [_ id]
    (or (.get jump-targets id)
      (throw (RuntimeException. (str "Could not find jump target " id))))))

(defn new-mcctx [mv]
  (->MethodCompilerCtx mv
    (java.util.HashMap.) (java.util.TreeSet.) 0
    (java.util.HashMap.)))

(defn emit-ast-node [ctx node]
  ((condp = (:node/kind node)
     :const-prim emit-const-prim
     :nil emit-nil
     :void (constantly nil)
     :do emit-do
     :local-use emit-local-use
     :assign-local emit-assign-local
     :arithmetic-2 emit-arithmetic-2
     :num-cmp-2 emit-num-cmp-2
     :if-true emit-if-true
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
     (throw (RuntimeException. (str "No handler for bytecode node " (pr-str node)))))
   ctx node))

(defn eval-ast [node]
  (let [prim (spec/prim? (:node/spec node))
        clsname "Eval" ;(gensym "Eval")
        cv (doto (ClassWriter. ClassWriter/COMPUTE_FRAMES)
             (.visit Opcodes/V19 Opcodes/ACC_PUBLIC (str "jl/run/" clsname)
               nil "java/lang/Object" nil))
        mv (.visitMethod cv (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
             "run" "()Ljava/lang/Object;" nil
             (into-array String ["java/lang/Exception"]))
        cl (clojure.lang.RT/makeClassLoader)
        sl (Label.)
        el (Label.)
        sl2 (Label.)
        el2 (Label.)]
    (doto mv
      (.visitCode)
      ; #_
      (do 
        (let [ctx (new-mcctx mv)]
          (emit-ast-node ctx node)
          (when prim
            (if (= "void" prim)
              (-emitInsn ctx :aconst-null)
              (emit-box ctx (type/box (type/prim-classname->type prim)))))))
      ; (.visitFrame Opcodes/F_SAME1 0 (object-array 0) 1 (object-array [Opcodes/NULL]))
      (.visitInsn Opcodes/ARETURN))
    
    (let [e (try
                  (.visitMaxs mv -1 -1)
                  (.visitEnd mv) nil
                  (catch Exception e e))]
      #_(java.nio.file.Files/write
        (java.nio.file.Path/of "tmp/eval.class" (make-array String 0))
        (.toByteArray cv)
        (make-array java.nio.file.OpenOption 0))
      (if e (throw e)
        (do (.defineClass cl (str "jl.run." clsname)
              (.toByteArray cv) nil)
          (let [p (promise)
                th (Thread/ofPlatform)]
            (.start th
              (fn [] (try (deliver p [:ok (eval '(jl.run.Eval/run))])
                       (catch Throwable ex (deliver p [:error ex])))))
            (let [rs (deref p 2000 [:timeout nil])]
              (match rs
                [:ok r] r
                [:error e'] (throw e')
                [:timeout _] (do (.stop th) ::timeout)))))))))

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









