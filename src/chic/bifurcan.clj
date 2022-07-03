(ns chic.bifurcan
  (:require
   [clojure.walk :as walk]
   [chic.util :as util]
   [better-cond.core :refer [cond] :rename {cond cond+}]
   [chic.types :as types]
   [clj-commons.primitive-math :as prim]
   [potemkin :refer [def-map-type unify-gensyms deftype+]])
  (:import
   (java.text CharacterIterator)
   (io.lacuna.bifurcan Map Rope)))

(declare ->WrappedMapTransient)

(def-map-type WrappedMap [^Map m mta]
  (get [_ k default-value]
       (.get m k default-value))
  (assoc [_ k v]
         (WrappedMap. (.put m k v) mta))
  (dissoc [_ k]
          (WrappedMap. (.remove m k) mta))
  (keys [_]
        (.keys m))
  (meta [_]
        mta)
  (with-meta [_ mta]
    (WrappedMap. m mta))
  clojure.lang.IEditableCollection
  (asTransient [_] (->WrappedMapTransient (.linear m))))

(deftype+ WrappedMapTransient [^Map m]
  potemkin.collections/PotemkinFn
  (invoke [_ k] (.get m k nil))
  (invoke [_ k not-found] (.get m k not-found))

  clojure.lang.ITransientMap
  (conj [self x]
        (cond+
         (instance? java.util.Map$Entry x)
         (let [e ^java.util.Map$Entry x]
           (.put m (.getKey e) (.getValue e)))
         (instance? clojure.lang.IPersistentVector x)
         (let [v ^clojure.lang.IPersistentVector x]
           (when-not (prim/== (util/compile (prim/int 2)) (.count v))
             (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
           (.put m (.nth v 0) (.nth v 1)))

         (loop [es (clojure.lang.RT/seq x)]
           (when-not (clojure.lang.Util/identical es nil)
             (let [e ^java.util.Map$Entry (.first es)]
               (.put m (.getKey e) (.getValue e))
               (recur (.next es))))))
        self)
  (assoc [self k v]
         (.put m k v)
         self)
  (without [self k]
           (.remove m k)
           self)
  (valAt [_ k] (.get m k nil))
  (valAt [_ k not-found] (.get m k not-found))
  (count [_] (.size m))
  (persistent [_]
              (->WrappedMap (.forked m) nil))

  clojure.lang.ITransientAssociative2
  (entryAt [_ k]
           (let [v (.get m k ::not-found)]
             (when-not (clojure.lang.Util/identical ::not-found v)
               (clojure.lang.MapEntry/create k v))))
  (containsKey [_ k] (.contains m k)))

(defmacro hmap [& kvs]
  (assert (or (== 1 (count kvs))
              (even? (count kvs))))
  `(->WrappedMap
    ~(if (seq kvs)
       (let [entries (if (== 1 (count kvs))
                       (seq (first kvs))
                       (partition 2 kvs))]
         (unify-gensyms
          `(let [m## (.linear (Map.))]
             ~@(map (fn [[k v]]
                      `(.put m## ~k ~v))
                    entries)
             (.forked m##))))
       `(Map.)) nil))

(defmacro hset [& items])
(defmacro vect [& items])

(defmacro with-colls [types & body]
  (let [conv-map (if (some #{:map} types)
                   (fn [form] `(hmap ~form))
                   identity)
        conv-set (if (some #{:set} types)
                   (fn [form] `(hset ~form))
                   identity)
        conv-vec (if (some #{:vec} types)
                   (fn [form] `(vect ~form))
                   identity)]
    `(do
       ~@(walk/postwalk
          (fn [form]
            (cond+
               (map? form)
               (conv-map form)
               (vector? form)
               (conv-vec form)
               (set? form)
               (conv-set form)
               form))
          body))))

(comment
  (type (keys (hmap {4 4})))
  (let [m (hmap :x 4 :some 435 "some" :butter)
        t (transient m)
        t (dissoc! t :x)]
    [m (persistent! t)])
  (type (first (seq (hmap 4 4))))
  (tap> (hmap :x :y ))

  #!
  )

(deftype RopeCharacterIterator
  [^Rope rope ^:unsynchronized-mutable ^int idx ^int end-idx]
  CharacterIterator
  (first [_]
    (set! idx (unchecked-int 0))
    ^char (.nth rope 0))
  (last [_]
    (set! idx (unchecked-dec-int end-idx))
    ^char (.nth rope idx))
  (current [_]
    (if (prim/== idx end-idx)
      CharacterIterator/DONE
      ^char (.nth rope idx)))
  (next [_]
    (set! idx (unchecked-inc-int idx))
    (if (prim/< idx end-idx)
      ^char (.nth rope idx)
      (do (set! idx end-idx)
          CharacterIterator/DONE)))
  (previous [_]
    (if (prim/zero? idx)
      CharacterIterator/DONE
      (do (set! idx (unchecked-dec-int idx))
          ^char (.nth rope idx))))
  (setIndex [_ idx2]
    (when (or (prim/< idx2 0) (prim/< end-idx idx2))
      (throw (IllegalArgumentException.
              (str "Index must be in interval [0, " end-idx
                   "] = [ getBeginIndex(), getEndIndex() ]"))))
    (set! idx idx2)
    (if (prim/== idx2 end-idx)
      CharacterIterator/DONE
      ^char (.nth rope idx2)))
  (getBeginIndex [_] 0)
  (getEndIndex [_] end-idx)
  (getIndex [_] idx)
  (clone [_]
    (RopeCharacterIterator. rope idx end-idx)))

(defn rope-character-iterator [^Rope rope]
  (let [size (.size rope)]
    (if (prim/< 0 size)
      (->RopeCharacterIterator rope 0 size)
      (types/->EmptyCharacterIterator))))
