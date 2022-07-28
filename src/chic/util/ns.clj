(ns chic.util.ns
  (:require
    [chic.util.impl.base :as impl.base]
    [clojure.data]
    [taoensso.encore :as enc]))

(defn -specs->mappings [specs]
  (reduce (fn [m spec]
            (cond
              (symbol? spec)
              (assoc m (symbol (name spec)) spec)
              :else (throw (Exception. "Invalid spec"))))
    {}
    specs))

(defn -on-src-var-changed [dst src _prev value]
  (alter-var-root dst (constantly value))
  (alter-meta! dst conj 
    (select-keys (meta src)
      [:doc :arglists :private :macro])))

(defn reset-var-inheritance! [id varmap]
  (alter-meta! *ns*
    (fn [mta]
      (let [prev-map (-> mta ::var-inheritances (get id))
            [removed-map new-map _] (clojure.data/diff prev-map varmap)]
        (reduce-kv (fn [_ dst src]
                     (ns-unmap (.-ns ^clojure.lang.Var dst) (impl.base/simple-symbol dst))
                     (remove-watch src dst))
          nil removed-map)
        (reduce-kv (fn [_ dst src]
                     (add-watch src dst #'-on-src-var-changed))
          nil new-map)
        (assoc-in mta [::var-inheritances id] varmap)))))

(defmacro inherit-vars [& specs]
  (let [id nil
        mappings (-specs->mappings specs)]
    `(do
       ~@(mapcat (fn [[sym srcsym]]
                   `[(def ~sym @(var ~srcsym))
                     (alter-meta! (var ~sym) enc/merge
                       (enc/-alias-meta (var ~srcsym)))])
           mappings)
       (let [varmap# ~(into {} (map #(do `[(var ~(key %)) (var ~(val %))]))
                       mappings)] 
         (reset-var-inheritance! ~id varmap#)
         varmap#))))
