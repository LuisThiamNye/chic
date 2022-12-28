(ns chic.util.ns
  (:require
    [chic.util.impl.base :as impl.base]
    [clojure.data]
    [taoensso.encore :as enc])
  (:import
    (clojure.lang Var)))

(defn -specs->mappings [specs]
  (reduce (fn [m spec]
            (cond
              (symbol? spec)
              (assoc m (symbol (name spec)) spec)
              :else (throw (Exception. "Invalid spec"))))
    {}
    specs))

(defn -propagate-var [src ^Var dst]
  (locking dst
    (let [mta (enc/merge (meta dst)
                (select-keys (meta src) [:doc :arglists :private :macro]))]
      (.bindRoot dst @src) ;; clears macro flag
      (reset-meta! dst mta))))

(defn -on-src-var-changed [^Var dst src _prev value]
  (when (ns-resolve (. dst -ns) (symbol dst))
    (-propagate-var src dst)))

(defn reset-var-inheritance! [id varmap]
  (alter-meta! *ns*
    (fn [mta]
      (let [prev-map (-> mta ::var-inheritances (get id))
            [removed-map new-map _] (clojure.data/diff prev-map varmap)]
        (reduce-kv 
          (fn* [_ ^Var dst src]
            (let [dst-sym (impl.base/simple-symbol dst)]
              (when (identical? dst (.getMapping (.-ns dst) dst-sym))
                (ns-unmap (.-ns dst) dst-sym)))
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
       ~@(map #(list 'def %) (keys mappings))
       (let [varmap# ~(into {} (map #(do `[(var ~(key %)) (var ~(val %))]))
                       mappings)] 
         (reduce-kv (fn* [_# dst# src#]
                      (-propagate-var src# dst#))
         nil varmap#)
         (reset-var-inheritance! ~id varmap#)
         varmap#))))

(defmacro deflink [sym srcsym]
  {:pre [(symbol? srcsym)]}
  `(do (declare ~sym)
     (-propagate-var (var ~srcsym) (var ~sym))
     (reset-var-inheritance!
       ~(symbol (name *ns*) (name sym))
       {(var ~sym) (var ~srcsym)})))

