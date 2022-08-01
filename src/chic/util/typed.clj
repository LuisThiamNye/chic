(ns chic.util.typed
  #_(:refer-clojure :only [])
  (:require
    [chic.util :as util :refer [loop-zip]])
  (:import
    (io.lacuna.bifurcan Map)))

(definterface ILookupImpl
  (get [o k])
  (get [o k nf])
  (getKeyCallSite [o k])
  (getKeyCallSite [o k nf]))

(def ^Map -class->lookup-impl (Map.))

(defn register-lookup-type! [^Class cls impl]
  (alter-var-root #'-class->lookup-impl
    (fn [^Map m]
      (let [clsname (.getName cls)
            m (loop-zip [^Class acls (.keys m)]
                [m m]
                (if (= clsname (.getName acls))
                  (.remove m acls)
                  (recur m))
                m)]
        (.put m cls impl)))))

(defn g* 
  ([o k] 
   (.get ^ILookupImpl (.get -class->lookup-impl (class o))
     o k))
  ([o k nf]
   (.get ^ILookupImpl (.get -class->lookup-impl (class o))
       o k nf)))

(defmacro g-
  ([o k]
   (let [cls (util/infer-form-tag o &env)
         impl ^ILookupImpl (.get -class->lookup-impl cls)
         o (vary-meta o assoc :tag (.getName cls))
         site (.getKeyCallSite impl o k)]
     (assert (not (identical? impl site)) 
       (str "Lookup not implemented for " cls))
     site))
  ([o k nf]
   (let [cls (util/infer-form-tag o &env)
         impl ^ILookupImpl (.get -class->lookup-impl cls)
         o (vary-meta o assoc :tag (.getName cls))
         site (.getKeyCallSite impl o k nf)]
     (assert (not (identical? impl site)) 
       (str "Lookup not implemented for " cls))
     site)))
