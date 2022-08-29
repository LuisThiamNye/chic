(ns jl.compiler.math
  (:require
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node]])
  (:import
    (org.objectweb.asm ClassVisitor MethodVisitor Opcodes ClassWriter)))

;; float, double, byte, short, int, long

"Adding:
- dadd
- fadd
- iadd
- ladd
Inc: 
- iinc (local int var) by another int

Subtraction:
isub
lsub
fsub
dsub
Multiplication:
imul
lmul
fmul
dmul
Division:
idiv
ldiv
fdiv
ddiv
Remainder:
irem
lrem
frem
drem
Negate:
ineg
lneg
fneg
dneg
Bitwise:
ishl
lshl
ishr - preserve sign
lshr - preserve sign
iushr
lushr

iand
land
ior
lor
ixor
lxor


JLS:
Widening primitive conversion (§5.1.2) is applied to convert either or both operands as specified by the following rules:

    If either operand is of type double, the other is converted to double.

    Otherwise, if either operand is of type float, the other is converted to float.

    Otherwise, if either operand is of type long, the other is converted to long.

    Otherwise, both operands are converted to type int.


"

#_(defn emit-add-d [^MethodVisitor mv x y]
  (-emit x) (-emit y)
  (.visitInsn mv Opcodes/DADD))

'(match [x y ret]
   [double T R] 
   [double double double]
   
   [T double R]
   [double double double]
   
   [float T R]
   [float float float]
   
   [T float R]
   [float float float]
   
   [long T R]
   [long long long]
   
   [T long R]
   [long long long]
   
   [T T #{byte short int}]
   [int int int]
   
   [T T long]
   [long long long]
   
   [T T float]
   [float float float]
   
   [T T double]
   [double double double]
   
   [T T R]
   [int int int])

(let [t1 Object
      t2 Object]
  (cond
    (= Double/TYPE t1)
    Double/TYPE
    (= Double/TYPE t2)
    (= Float/TYPE t1)
    (= Float/TYPE t2)
    (= Long/TYPE t1)
    (= Long/TYPE t2)
    :else))
#_
(match [Object Object]
  [(d :guard (partial = Double/TYPE)) o]
  
  [o (d :guard (partial = Double/TYPE))]
  
  [(d :guard (partial = Float/TYPE)) o]
  
  [o (d :guard (partial = Float/TYPE))]
  
  [(d :guard (partial = Long/TYPE)) o]
  
  [o (d :guard (partial = Long/TYPE))]
  
  )

'(+ 1 1 1N) '(+ (+ 1 1) 1N) '(+ int int BigInt)
'(+ 1 1N)
;; adjacent num literals should be added, if same type, no overflow
;; gets very complex considering all permutations
;; just do simple approach
#_ {:spec/kind :fn
     :fn (fn [this]
           ;; need casting info too
           (negotiate-addition-operand-type next-param-type this))}

'(+ 1N 1N)
;; get constructor from known type for ambiguous literal

(defn -negotiate-add-subject-spec [trait spec r]
  (condp = (:implname trait)
    "java.lang.BigDecimal"
    (when-some [s (spec/try-coerce-class "java.lang.BigDecimal" spec)]
      (when-some [r' (spec/try-coerce-class "java.lang.BigDecimal" r)]
        [s r' {:emitter '?}]))
    "java.lang.BigInteger"
    (when-some [s (spec/try-coerce-class "java.lang.BigInteger" spec)]
      (when-some [r' (spec/try-coerce-class "java.lang.BigInteger" r)]
        [s r' {:emitter '?}]))))

(defn negotiate-addition-specs* [s1 s2 r]
  (or (when (spec/certain-trait? :primnum-like s1)
        (when-some [s2' (spec/try-coerce-trait :primnum-like s2)]
          (when-some [r' (spec/try-coerce-trait :primnum-like r)]
            ;; prim add, maybe unbox
            [s1 s2' r' {:emitter '?}])))
    (when-some [t (spec/get-certain-trait s1 :addition)]
      (when-some [[s2' r' info] (-negotiate-add-subject-spec t s2 r)]
        ;; a.add(b)
        [s1 s2' r' info]))))

(defn negotiate-addition-specs [s1 s2 r]
  (or (negotiate-addition-specs* s1 s2 r)
    (let [ret (negotiate-addition-specs* s2 s1 r)]
      (update ret (dec (count ret)) assoc :swap? true))))
#_
(defn anasf-add [{:keys [children] :as node}]
  (let [x1 (-analyse-node (nth children 1))
        x2 (-analyse-node (nth children 2))
        r {:node/kind :special-form
           ; :node/spec {:spec/kind }
           :emitter '?}
        ms {:spec/kind :fn
            :fn (fn [[x1-spec x2-spec r]]
                  (negotiate-addition-specs x1-spec x2-spec r))}
        [x1 x2 r] (spec/link-multispec ms [x1 x2 r])]
    ; {:spec/kind :capabilities
    ;  :capabilities [:addition]}
    ; ;; 3 unknown thing: x1-type, method, x2-type
    ; ;; multispec
    ; {:spec/kind :alt
    ;  :alts [#_{:spec/kind :capabilities ;; prim num or unboxable
    ;          :capabilities [:primnum-like]}]}
    
    
    (assoc r :args [x1 x2])))
(defn anasf-arith-2 [op {:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    {:node/kind :arithmetic-2
     :op op :type :int
     :node/spec (:node/spec x2)
     :arg1 x1 :arg2 x2
     ; :args [x1 x2]
     :node/locals (:node/locals x2)}))
(defn anasf-add [node]
  (anasf-arith-2 :add node))
(defn anasf-subtract [node]
  (anasf-arith-2 :subtract node))
(defn anasf-multiply [node]
  (anasf-arith-2 :multiply node))
(defn anasf-divide [node]
  (anasf-arith-2 :divide node))
(defn anasf-remainder [node]
  (anasf-arith-2 :remainder node))
(defn anasf-bit-and [node]
  (anasf-arith-2 :and node))
(defn anasf-bit-or [node]
  (anasf-arith-2 :or node))
(defn anasf-bit-xor [node]
  (anasf-arith-2 :xor node))
(defn anasf-inc [{:keys [children]:as node}]
  (assert (= 2 (count children)))
  (let [[x] (ana/analyse-args node (subvec children 1))]
    {:node/kind :arithmetic-2
     :op :add :type :int
     :node/spec (:node/spec x)
     :arg1 x :arg2 (ana/new-const-prim-node nil 1)
     ; :args [x1 x2]
     :node/locals (:node/locals x)}))
(swap! ana/*sf-analysers assoc "+" #'anasf-add)
(swap! ana/*sf-analysers assoc "-" #'anasf-subtract)
(swap! ana/*sf-analysers assoc "*" #'anasf-multiply)
(swap! ana/*sf-analysers assoc "/" #'anasf-divide)
(swap! ana/*sf-analysers assoc "rem" #'anasf-remainder)
(swap! ana/*sf-analysers assoc "inc" #'anasf-inc)
(swap! ana/*sf-analysers assoc "bit-and" #'anasf-bit-and)
(swap! ana/*sf-analysers assoc "bit-or" #'anasf-bit-or)
(swap! ana/*sf-analysers assoc "bit-xor" #'anasf-bit-xor)


#_
(defn add-sf [ctx form]
  (if (= 3 (count form))
    (let [x1 (nth form 1)
          x2 (nth form 2)
          r1 (-emitForm ctx x1)
          r2 (-emitForm ctx x2)
          rt (-resolveArithmeticTypes ctx r1 r2)]
      
      )
    "TODO"))

