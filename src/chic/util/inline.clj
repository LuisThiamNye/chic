(ns chic.util.inline
  (:require
   [clojure.walk :as walk]))

(defn swap-let-binding-input-syms [smap bindings]
  (:bindings
   (reduce
    (fn [{:keys [remaps bindings] :as acc} [sym vexpr]]
      (let [vexpr (walk/postwalk-replace remaps vexpr)]
        (if (contains? remaps sym)
          {:bindings (conj bindings [sym vexpr])
           :remaps (dissoc remaps sym)}
          (update acc :bindings conj [sym vexpr]))))
    {:remaps smap :bindings []}
    bindings)))

(defmacro fnlet-snippet [fexpr]
  (let [input-syms (some #(when (vector? %) %) fexpr)
        letexpr (last fexpr)
        ret-syms (last letexpr)
        ret-syms-set (set ret-syms)
        bindings (:bindings
                  (reduce
                   (fn [{:keys [remaps bindings] :as acc} [sym vexpr :as pair]]
                     (let [vexpr (walk/postwalk-replace remaps vexpr)]
                       (if (contains? ret-syms-set sym)
                         (update acc :bindings conj [sym vexpr])
                         (let [sym2 (gensym (str sym "_PRIV"))]
                           {:bindings (conj bindings [sym2 vexpr])
                            :remaps (assoc remaps sym sym2)}))))
                   {:remaps {} :bindings []}
                   (eduction (partition-all 2) (second letexpr))))]
    `(quote {:input-syms ~input-syms
             :bindings ~bindings
             :ret-syms ~ret-syms-set})))

(defmacro inline-fnsnip-multiretmap [varsym direct-syms & [smap]]
  (let [{:keys [bindings input-syms ret-syms]} @(resolve varsym)
        smap (or smap {})
        input-smap (update-keys smap symbol)]
    (assert (= (set direct-syms)
               (set (remove #(contains? input-smap %) input-syms))))
    `(let ~(into [] cat
                 (swap-let-binding-input-syms input-smap bindings))
       ~(zipmap (map keyword ret-syms) ret-syms))))
