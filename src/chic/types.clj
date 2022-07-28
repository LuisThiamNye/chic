(ns chic.types
  (:require
    [chic.util :as util :refer [deftype+]]
   [clj-commons.primitive-math :as prim])
  (:import
    (clojure.lang Murmur3)
   (java.text CharacterIterator)
   (io.github.humbleui.types IPoint)))

(definterface IXyI 
  (^int getX [])
  (^int getY []))

(defprotocol PXyIM
  (reset-xyi [_ x y])
  (to-ipoint [_]))

(definterface IXyIM
  (resetXy [^int x ^int y]))

(deftype+ XyIMunsync [^:mut ^int x ^:mut ^int y]
  :keys true
  chic.util.IPrintableObjectFields
  ; clojure.lang.IHashEq
  ; (hasheq [self]
  ;   (bit-xor (util/compile (hash "XyIMunsync"))
  ;     (unchecked-add-int
  ;       (Murmur3/hashInt x) (Murmur3/hashInt y))))
  ; (hashCode [self]
  ;   (unchecked-add-int x y))
  ; (equals [self other]
  ;   (or (identical? self other)
  ;     (and (instance? IXyI other)
  ;       (= x (:x other))
  ;       (= y (:y other)))))
  IXyI
  (getX [_] x)
  (getY [_] y)
  IXyIM
  (resetXy [_ x' y'] (set! x x') (set! y y'))
  PXyIM
  (reset-xyi [_ x2 y2] (set! x (int x2)) (set! y (int y2)))
  (to-ipoint [_] (IPoint. x y))
  clojure.lang.IDeref
  (deref [_] (IPoint. x y)))

(:y (->XyIMunsync 1 1))

(deftype EmptyCharacterIterator []
  CharacterIterator
  (first [_]
    CharacterIterator/DONE)
  (last [_]
    CharacterIterator/DONE)
  (current [_]
    CharacterIterator/DONE)
  (next [_]
    CharacterIterator/DONE)
  (previous [_]
    CharacterIterator/DONE)
  (setIndex [_ idx2]
    (when (prim/not== idx2 0)
      (throw (IllegalArgumentException. "Index must be zero")))
    CharacterIterator/DONE)
  (getBeginIndex [_] 0)
  (getEndIndex [_] 0)
  (getIndex [_] 0)
  (clone [self] self))
