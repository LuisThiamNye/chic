(ns chic.util.impl.loopr
  (:require
    [chic.util.impl.base :refer [<- tag-class]]
    [chic.util.impl.analyzer :as impl.ana]
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
      ([[_env bindings]] bindings)
      ([[env bindings] [sym coll-expr]]
       (let [coll-tag (tag-class (impl.ana/infer-tag coll-expr env))
             item-meta-tag (:tag (meta sym))
             item-tag (if item-meta-tag
                        (tag-class item-meta-tag)
                        (if (.isArray coll-tag)
                          (.componentType coll-tag)
                          Object))
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

(defn analyze-acc-bindvec [env bindvec]
  (transduce (partitionv-all 2)
    (fn
      ([[_env bindings]] bindings)
      ([[env bindings] [sym init]]
       (let [tag (impl.ana/infer-tag init env)]
         [(assoc env sym {:tag tag})
          (conj bindings
            {:sym sym :tag tag :init init})])))
    [env []] bindvec))

(comment
  (analyze-acc-bindvec {} '[x [1 2 3]])
  (analyze-acc-bindvec {} '[])
  )

(defn reducify-loop-ast [xacc-syms ast]
  (impl.ana/walk-tails
    (fn [node]
      (if (= :recur (:op node))
        #_(let [recmap (:form (first (:exprs node)))]
            {:op :const
             :form `(do)} )
        (let [exprs (:exprs node)
              racc (nth exprs 0 nil)
              xaccs (enc/vnext exprs)]
          {:op :const
           :form `(do ~@(map-indexed 
                          (fn [i expr]
                            `(.set ~(nth xacc-syms i) ~(:form expr)))
                          xaccs)
                    ~(:form racc))})
        {:op :const
         :form `(reduced ~(:form node))}))
    ast))

(defn reducify-loop-form [accinfos form]
  (@(first (impl.ana/jvm-passes ['emit-form]))
    (reducify-loop-ast 
      (mapv :sym (enc/vnext accinfos))
      (impl.ana/build-ast form
        (assoc (dissoc (ana/empty-env) :no-recur)
          :context :ctx/return
          :loop-locals (count accinfos)
          :loop-id (gensym "loopr_"))))))

(defn loopr-ireduceinit [accinfos loop-expr coll item-sym]
  (let [acc-syms (mapv :sym accinfos)
        xacc-syms (enc/vnext acc-syms)
        racc-sym (nth acc-syms 0 '__)
        reduce-body (reducify-loop-form accinfos loop-expr)
        rf-sym (gensym "rf_")]
    `(let* [~@(mapcat (fn [{:keys [sym tag]}]
                        [sym (or (when tag
                                   (case (.getName ^Class tag)
                                     ("long" "int" "short") `(proteus.Containers$L. ~sym)
                                     ("double" "float") `(proteus.Containers$D. ~sym)
                                     "byte" `(proteus.Containers$B. ~sym)))
                               `(proteus.Containers$O. ~sym))])
                (enc/vnext accinfos))
            ~rf-sym (fn [~racc-sym ~item-sym]
                      (let* [~@(mapcat (fn [sym]
                                         [sym `(.-x ~sym)])
                                 xacc-syms)]
                        ~reduce-body))
            ~racc-sym (.reduce ~coll ~rf-sym ~(:init (nth accinfos 0 nil)))
            ~@(mapcat (fn [sym]
                        [sym `(.-x ~sym)])
                xacc-syms)]
       )))

(defn arrayify-loop-ast [idx-sym ast]
  (impl.ana/walk-tails
    (fn [node]
      (if (= :recur (:op node))
        (update node :exprs conj
          {:op :const
           :form `(unchecked-inc ~idx-sym)})
        node))
    ast))

(defn arrayify-loop-form [accinfos idx-sym form]
  (@(first (impl.ana/jvm-passes ['emit-form]))
    (arrayify-loop-ast 
      idx-sym
      (impl.ana/build-ast form
        (assoc (dissoc (ana/empty-env) :no-recur)
          :context :ctx/return
          :loop-locals (count accinfos)
          :loop-id (gensym "loopr_"))))))

(comment
  (arrayify-loop-form [1 2] 'idx '(do (recur 1 2))))

(defn loopr-array* [accinfos loop-expr ary ^Class item-tag item-sym then-form]
  (let [acc-syms (mapv :sym accinfos)
        i-sym (gensym "i_") ary-sym (gensym "ary_")]
    `(let* [~ary-sym ~ary
            n# (alength ~ary-sym)]
       (loop [~@(mapcat vector acc-syms acc-syms)
              ~i-sym ~(int 0)]
         (if (< ~i-sym n#)
           (let* [~item-sym ~(with-meta `(aget ~ary-sym ~i-sym)
                               {:tag (symbol (.getName item-tag))})]
             ~(arrayify-loop-form accinfos i-sym loop-expr))
           ~then-form)))))

(defn loopr* [env coll-bindvec acc-bindvec loop-expr completer-expr]
  (let [accinfos (analyze-acc-bindvec env acc-bindvec)
        collinfos (analyze-coll-bindvec env coll-bindvec)
        collinfo (first collinfos)
        tag (:coll-tag collinfo)]
    (cond
      (and tag (.isArray tag))
      `(let ~acc-bindvec
         ~(loopr-array* accinfos loop-expr (second coll-bindvec) (:item-tag collinfo)
             (first coll-bindvec) completer-expr))
      (isa? tag clojure.lang.IReduceInit)
      `(<-
           (let ~acc-bindvec)
           ~(loopr-ireduceinit accinfos loop-expr (second coll-bindvec) (first coll-bindvec))
           ~completer-expr))))

(defmacro loopr [coll-bindvec acc-bindvec loop-expr completer-expr]
  (loopr* &env coll-bindvec acc-bindvec loop-expr completer-expr))

(comment
  (loopr [x [1 2 3]]
    [i 0]
    (let []
      (recur (inc i)))
    i #_x)
  (ana/macroexpand-1 
    '(loopr [x ^ints (into-array [1 2 3])]
      [i {}
       b nil]
      (let []
        (recur {} nil))
      b #_x))
  )

#_(defn zip-* [ary-syms iter-syms red-syms]
    (reify clojure.lang.IReduceInit
      (reduce [_ rf init]
        (loop [acc init]
          (let [ret (rf acc x)]
            (if (reduced? ret)
              @ret
              (recur ...)))))))
