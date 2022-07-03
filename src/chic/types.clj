(ns chic.types
  (:require
   [clj-commons.primitive-math :as prim])
  (:import
   (java.text CharacterIterator)
   (io.github.humbleui.types IPoint)))

(defprotocol PXyIM
  (reset-xyi [_ x y])
  (to-ipoint [_]))

(deftype XyIMunsync [^:unsynchronized-mutable ^int x
                     ^:unsynchronized-mutable ^int y]
  PXyIM
  (reset-xyi [_ x2 y2] (set! x (int x2)) (set! y (int y2)))
  (to-ipoint [_] (IPoint. x y))

  clojure.lang.ILookup
  (valAt [_ k notFound]
    (case k
      :x x :y y
      notFound))
  (valAt [_ k]
    (case k
      :x x :y y
      nil)))

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
