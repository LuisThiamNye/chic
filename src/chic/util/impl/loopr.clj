(ns chic.util.impl.loopr
  (:require
    [chic.util.impl.base :refer [<- tag-class cond-class-isa]]
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
    seq-reduce->internal-reduce
    clojure.lang.LazySeq
    seq-reduce->chunked-seq
    clojure.lang.PersistentVector
    seq-reduce->chunked-seq
    Iterable
    iter-reduce
    clojure.lang.APersistentMap$KeySeq
    iter-reduce
    clojure.lang.APersistentMap$ValSeq
    iter-reduce)

(defn determine-iter-method [coll-tag]
  (<- (if (or (nil? coll-tag) (identical? Object coll-tag))
        :iterable)
    (if (.isArray coll-tag)
      :array)
    (cond-class-isa coll-tag
      java.util.Iterator :iterator
      Iterable :iterable)))

(defn analyze-coll-bindvec [env bindvec]
  #_[{:item-expr 'expr :coll-expr 'expr
      :item-tag Class :coll-tag Class
      :iter-method 'kw}]
  (transduce (partitionv-all 2)
    (fn
      ([[_env bindings]] bindings)
      ([[env bindings] [item-expr coll-expr]]
       (if (= :idx coll-expr)
         [env (update bindings (dec (count bindings))
                assoc :idx-sym item-expr)]
         (let [coll-tag (impl.ana/infer-form-tag coll-expr env)
               item-meta-tag (:tag (meta item-expr))
               item-tag (if item-meta-tag
                          (tag-class item-meta-tag)
                          (if (and coll-tag (.isArray coll-tag))
                            (.componentType coll-tag)
                            Object))]
           [(assoc env item-expr {:tag item-tag})
            (conj bindings
              {:item-expr item-expr :coll-expr coll-expr
               :item-tag item-tag :coll-tag coll-tag
               :iter-method (determine-iter-method coll-tag)})]))))
    [env []] bindvec))

(comment
  (analyze-coll-bindvec {} '[x [1 2 3]])
  )

(defn analyze-acc-bindvec [env bindvec]
  #_[{:sym 'sym :tag Class :init 'expr}]
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

(defn arrayify-loop-form2 [form idx-sym]
  ((fn* -arrayify [form]
     (if (seq? form)
       (let [f (first form)
             form' 
             (or (when (symbol? f)
                   (let [nam (name f)
                         ns (namespace f)]
                     (cond (and
                             (= "recur" nam)
                             (or (nil? ns) (= 'clojure.core ns)))
                       (concat form (list (list `unchecked-inc-int idx-sym)))
                       (and (= "quote" nam)
                         (or (nil? ns) (= 'clojure.core ns)))
                       form)))
               (list* (map -arrayify form)))]
         (with-meta form' (meta form)))
       form))
   form))

(comment
  (arrayify-loop-form [1 2] 'idx 
    '(do '(recur)
       (recur 1 2)))
  (arrayify-loop-form2
    '(do '(recur)
       (recur 1 2)) 'idx)
  (meta (arrayify-loop-form2 '(x) 'idx))
  
  )

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

(defmacro min-of-ints [form1 & forms]
  (let* [acc (gensym "smallest_")
         forms (vec forms)
         nextras (count forms)
         cexpr (fn [form acc]
                   (let [form' (if (symbol? form) form (gensym form))]
                     (list 'if (list `< form' acc)
                       form' acc)))]
    (<- (if (zero? nextras) form1)
      (let* [comp1 (cexpr (nth forms 0) form1)])
      (if (= 1 nextras) comp1)
      (list 'let*
        (into [acc comp1]
          (mapcat (fn [form]
                    [acc (cexpr form acc)]))
          (when (< 1 nextras) (subvec (pop forms) 1)))
        (cexpr (peek forms) acc)))))

(defn loop-zip* [env coll-bindvec acc-bindvec loop-expr completer-expr]
  (let [;accinfos (analyze-acc-bindvec &env acc-bindvec)
        collinfos (analyze-coll-bindvec env coll-bindvec)
        ary-bindings (java.util.ArrayList.)
        it-bindings (java.util.ArrayList.)
        *item-letvec (volatile! (transient []))
        defined-idx-sym (:idx-sym (peek collinfos))
        add-it-binding (fn [item-expr b]
                         (vswap! *item-letvec conj! item-expr)
                         (vswap! *item-letvec conj! (list '. (:sym b) 'next))
                         (.add it-bindings b))
        i-sym (or defined-idx-sym (gensym "idx_"))
        _ (reduce
            (fn [_ {:keys [coll-expr ^Class coll-tag iter-method item-expr idx-sym] :as info}]
              (when (and idx-sym (not (identical? i-sym idx-sym)))
                (throw (ex-info "Cannot bind idx sym here" {})))
              (case iter-method
                :iterator                     
                (add-it-binding item-expr
                  {:sym (if (symbol? coll-expr) 
                          coll-expr (gensym "iterator"))
                   :init coll-expr})
                :iterable
                (add-it-binding item-expr
                  {:sym (gensym "iterator")
                   :init
                   (list '.
                     (vary-meta coll-expr assoc :tag
                       (if (or (nil? coll-tag) (identical? Object coll-tag))
                         "Iterable"
                         (.getName coll-tag)))
                     'iterator)})
                :array
                (let* [ary-sym (if (symbol? coll-expr)
                             coll-expr (gensym "ary_"))]
                  (vswap! *item-letvec conj! item-expr)
                  (vswap! *item-letvec conj! (list `aget ary-sym i-sym))
                  (.add ary-bindings
                    (assoc info :coll-sym ary-sym)))
                (throw (ex-info "Coll not iterable" 
                         {:coll-expr coll-expr
                          :coll-tag coll-tag}))))
            nil
            collinfos)
        ary? (pos? (count ary-bindings))
        ary-n (gensym "ary-n_")
        continue (list* `and 
                   (cond->>
                     (map #(list '. (:sym %) 'hasNext)
                       it-bindings)
                     ary?
                     (cons (list `< i-sym ary-n))))
        track-idx? (or ary? defined-idx-sym)]
    (list 'let*
      (-> (if ary?
            (-> []
              ;; bind array to symbol
              (into (comp (keep (fn [{:keys [coll-sym coll-expr]}]
                                  (when-not (identical? coll-sym coll-expr)
                                    [coll-sym coll-expr])))
                      cat)
                ary-bindings)
              ;; smallest array size
              (into [ary-n (list* `min-of-ints
                             (map (fn [{:keys [coll-sym]}]
                                    (list `alength coll-sym))
                               ary-bindings))]))
            [])
        ;; Iterator bindings
        (into (comp
                (keep (fn [{:keys [sym init]}] 
                        (when-not (identical? sym init)
                          [sym init])))
                cat)
          it-bindings))
       (list `loop (cond-> acc-bindvec track-idx?
                     (-> (conj i-sym) (conj (int 0))))
         (list `if continue
           (list `let (persistent! @*item-letvec)
             (cond-> loop-expr track-idx? (arrayify-loop-form2 i-sym)))
           completer-expr)))))

(defmacro loop-zip 
  ([coll-bindvec] (loop-zip* &env coll-bindvec [] `(recur) nil))
  ([coll-bindvec loop-expr] 
   (loop-zip* &env coll-bindvec [] loop-expr nil))
  ([coll-bindvec acc-or-loop loop-or-comp]
   (if (vector? acc-or-loop)
     (loop-zip* &env coll-bindvec acc-or-loop loop-or-comp nil)
     (loop-zip* &env coll-bindvec [] acc-or-loop loop-or-comp)))
  ([coll-bindvec acc-bindvec loop-expr completer-expr]
   (loop-zip* &env coll-bindvec acc-bindvec loop-expr completer-expr)))

(comment
  (rwalk/macroexpand-all
    '(let [idx (Object.)]
       (loop-zip [x ^objects idk]
          )))
  
  (chic.debug/puget-prn
    (analyze-coll-bindvec {} '[x (object-array [1 2 3])]))

  )

#_(defn zip-* [ary-syms iter-syms red-syms]
    (reify clojure.lang.IReduceInit
      (reduce [_ rf init]
        (loop [acc init]
          (let [ret (rf acc x)]
            (if (reduced? ret)
              @ret
              (recur ...)))))))
