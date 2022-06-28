(ns chic.types
  (:import
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
