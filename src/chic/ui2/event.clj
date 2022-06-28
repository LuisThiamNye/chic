(ns chic.ui2.event
  #_(:require
     [])
  (:import
   (io.github.humbleui.jwm MouseButton EventMouseButton)))

(def kw->mouse-button
  {:primary MouseButton/PRIMARY
   :secondary MouseButton/SECONDARY
   :middle MouseButton/MIDDLE
   :back MouseButton/BACK
   :FORWARD MouseButton/FORWARD})

(defmacro case-mousebtn [e & clauses]
  (let [kw->mouse-button-ord #(.ordinal ^MouseButton (kw->mouse-button %))
        xf-test (fn [expr]
                  (cond
                    (keyword? expr)
                    (kw->mouse-button-ord expr)
                    (seq? expr)
                    (map kw->mouse-button-ord expr)))
        pairs (map (fn [[test then]]
                     [(xf-test test) then])
                   (partition 2 clauses))
        fallback (when (odd? (count clauses))
                   (last clauses))
        e (vary-meta e assoc :tag (symbol (.getName EventMouseButton)))]
    `(case (.ordinal (.getButton ~e))
       ~@(apply concat pairs)
       ~fallback)))

#_(defn mousedown-handler [{}]
    (fn [ctx rect evt]))
