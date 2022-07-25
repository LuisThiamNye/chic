(ns chic.util.loopr
  (:require
    [chic.util :as util]
    [chic.util.analyzer :as util.ana]
    [clojure.walk :as walk]
    [taoensso.encore :as enc]
    [tech.droit.fset :as fset]
    [clojure.tools.analyzer.jvm :as ana]
    [potemkin :refer [unify-gensyms doit]]
    [riddley.walk :as rwalk]))

#_(clojure.lang.IReduceInit
    .reduce
    clojure.lang.IReduce
    .reduce
    nil
    nil
    clojure.lang.ASeq
    seq-reduce
    clojure.lang.LazySeq
    seq-reduce
    clojure.lang.PersistentVector
    seq-reduce
    Iterable
    iter-reduce
    clojure.lang.APersistentMap$KeySeq
    iter-reduce
    clojure.lang.APersistentMap$ValSeq
    iter-reduce)

(defn analyze-coll-bindvec [env bindvec]
  (transduce (partitionv-all 2)
    (fn
      ([[env bindings]] bindings)
      ([[env bindings] [sym coll-expr]]
       (let [item-tag (util/get-sym-tag sym)
             coll-tag (util/infer-tag coll-expr env)
             loop-method nil]
         [(assoc env sym {:tag item-tag})
          (conj bindings
            {:sym sym :coll-expr coll-expr
             :item-tag item-tag :coll-tag coll-tag
             :loop-method loop-method})])))
    [env []] bindvec))

(comment
  (analyze-coll-bindvec {} '[x [1 2 3]])
  )

(defn reducify-loop-ast [ast]
  (util.ana/walk-tails
    (fn [node]
      (prn (:op node))
      (when (= :recur (:op node))
        (prn "recur"))
      node)
    ast))

(comment
  (reducify-loop-ast (util.ana/build-ast '(case 1 1 (recur))
                       (assoc (ana/empty-env) :context :ctx/return
                         :loop-locals 0)))
  
  )

(defn loopr-ireduceinit []
  (.reduce 
    coll
    (fn [_acc item]
      (doreduce item))
    nil))

(defn loopr* [coll-bindvec acc-bindvec loop-expr completer-expr])

(loopr [x [1 2 3]]
  (recur (inc x))
  x)

#_(defn zip-* [ary-syms iter-syms red-syms]
  (reify clojure.lang.IReduceInit
    (reduce [_ rf init]
      (loop [acc init]
        (let [ret (rf acc x)]
          (if (reduced? ret)
            @ret
            (recur ...)))))))
