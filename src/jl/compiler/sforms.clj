(ns jl.compiler.sforms
  (:require
    [chic.util :refer [inherit-vars loopr loop-zip <-]]
    [jl.interop :as interop]
    [jl.compiler.defclass :as defclass]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node coerce-to-class]]
    [jl.compiler.type :as type]
    [jl.rv-data :as rv-data])
  (:import
    (org.objectweb.asm Type)))

(inherit-vars defclass/anasf-defclass)

#_(defn anasf-isnil [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    {:node/kind :unary-cmp
     }))

(defn coerce-arith-pair [env x1 x2]
  (let [clsname1 (spec/get-exact-class (:node/spec x1))
        clsname2 (spec/get-exact-class (:node/spec x2))
        prim1? (type/prim-classname->type clsname1)
        prim2? (type/prim-classname->type clsname2)
        ; _ (assert (and prim1? prim2?)
        ;     (str "types " clsname1 " " clsname2 " not primitive"))
        ]
    (if (= clsname1 clsname2)
      [x1 x2 (cond
               prim1? (keyword clsname1)
               prim2? (keyword clsname2)
               :else (throw (ex-info (str "types " clsname1
                                       " " clsname2 " not primitive") {})))]
      (let [cls1 (interop/resolve-class env clsname1)
            cls2 (interop/resolve-class env clsname2)
            c1 (ana/get-coercion env cls1 cls2)]
        (if (and c1 prim2?)
          [(assoc x1 :node/coercion c1) x2 (keyword clsname2)]
          (let [c2 (ana/get-coercion env cls2 cls1)]
            (if (and c2 prim1?)
              [x1 (assoc x2 :node/coercion c2) (keyword clsname1)]
              (throw (ex-info (str "No coercion to unify " clsname1 ", " clsname2) {})))))))))

;; TODO integer optimisations eg (< -1 i) => (<= 0 i) - compare to zero
(defn anasf-numcmp [op {:keys [children node/env] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))
        [x1 x2 prim] (coerce-arith-pair env x1 x2)]
    (ana/transfer-branch-env x2
      {:node/kind :num-cmp-2
       :node/spec (spec/of-class "boolean")
       :op op
       :type prim
       :arg1 x1 :arg2 x2})))
(defn anasf-gte [node] (anasf-numcmp :>= node))
(defn anasf-gt [node] (anasf-numcmp :> node))
(defn anasf-lte [node] (anasf-numcmp :<= node))
(defn anasf-lt [node] (anasf-numcmp :< node))
(defn anasf-eq [node] (anasf-numcmp := node))

(defn anasf-identical? [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))
        clsname1 (spec/get-exact-class (:node/spec x1))
        clsname2 (spec/get-exact-class (:node/spec x2))
        prim1? (type/prim-classname->type clsname1)
        prim2? (type/prim-classname->type clsname2)
        prim? prim1?
        _ (assert (not (or prim1? prim2?))
            (str "types " clsname1 " " clsname2 " must not be primitive"))]
    (ana/transfer-branch-env x2
      {:node/kind :num-cmp-2
       :node/spec (spec/of-class "boolean")
       :op :=
       :type :ref
       :arg1 x1 :arg2 x2})))

;; TODO auto convert shortcutting 'and' to non-shortcutting bit-and if operands are cheap

(defn join-branch-specs [nodes]
  (let [specs (into [](comp (remove nil?)
                        (map :node/spec)
                        (remove #(= :jump (:spec/kind %)))
                        (distinct))
                nodes)]
    (if (empty? specs)
      {:spec/kind :jump}
      (if (= 1 (count specs))
        (first specs)
        {:spec/kind :union
         :specs specs}))))

(defn coerce-join-branches [env nodes]
  ;; need to agree on a primitive type
  (let [classes (mapv (comp (partial spec/get-duck-class env) :node/spec) nodes)
        nonvoids (filter (complement #(= Void/TYPE %)) classes)]
    (if (empty? nonvoids)
      [nodes (spec/of-class "void")]
      (if (empty? (remove interop/jump-class? classes))
        [nodes {:spec/kind :jump}]
        (let [ref-type?
             (boolean (seq (filter #(and
                                      (not (interop/jump-class? %))
                                      (or (not (interop/-primitive? %))
                                        (= Void/TYPE %)))
                             classes)))]
         (if ref-type?
           (let [nodes'
                 (mapv (fn [node cls]
                         (<-
                           (if (= Void/TYPE cls)
                             (assoc node :node/coercion interop/void->nil-conversion))
                           (if (interop/jump-class? cls) node)
                           (if (interop/-primitive? cls)
                             (let [co (ana/prim-class->box-conversion cls)]
                               (assoc node
                                 :node/coercion co
                                 :node/spec
                                 (spec/of-class
                                   (type/get-classname
                                     (type/box (Type/getType cls)))))))
                           node))
                   nodes classes)
                 spec (join-branch-specs nodes')]
             [nodes' spec])
           (let [widest (first (remove interop/jump-class? nonvoids)) ;; FIXME
                 spec (spec/of-class (interop/-getName widest))]
             [(mapv (fn [node cls]
                      (<-
                        (if (= Void/TYPE cls)
                          (throw (ex-info "can't mix prim + void types" {})))
                        (if (interop/jump-class? cls) node)
                        (coerce-to-class widest node)))
                nodes classes)
              spec])))))))

;; TODO detect mergeable ifs (if (if t t (do 3 f)) 1 2) -> (if t 1 (do 3 2))
(defn anasf-if [{:keys [children] :as node}]
  (assert (= 4 (count children)))
  (let [test (ana/analyse-expr node (nth children 1))
        then (ana/analyse-after test (nth children 2))
        else (ana/analyse-after then (nth children 3))
        env (:node/env else)
        [[then else] s] (coerce-join-branches env [then else])]
    ; (throw (RuntimeException.
    ;          (str "Unmatching if specs:\nif: "
    ;            (pr-str sthen) "\nelse: " (pr-str selse))))
    (ana/transfer-branch-env else
      {:node/kind :if-true
       :node/spec s
       :test test
       :then then
       :else else})))

(defn anasf-and [{:keys [children] :as node}]
  ;; TODO better impl
  ((fn self- [prev children]
     (if children
       (let [test (ana/analyse-expr prev (first children))]
         (ana/transfer-branch-env prev
           {:node/kind :if-true
            :node/spec (spec/of-class "boolean")
            :test test
            :then (self- test (next children))
            :else (ana/new-const-prim-node test false)}))
       (ana/new-const-prim-node prev true)))
   node (next children)))

(defn anasf-or [{:keys [children] :as node}]
  ;; TODO better impl
  ((fn self- [prev children]
     (if children
       (let [test (ana/analyse-expr prev (first children))]
         (ana/transfer-branch-env prev
           {:node/kind :if-true
            :node/spec (spec/of-class "boolean")
            :test test
            :then (ana/new-const-prim-node test true)
            :else (self- test (next children))}))
       (ana/new-const-prim-node prev false)))
   node (next children)))

(defn anasf-case-int [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [test (coerce-to-class Integer/TYPE (ana/analyse-expr node (nth children 1)))
        {:keys [keymap cases]}
        (reduce (fn [acc [keydecl then]]
                  (let [keydecl->int (fn [keydecl]
                                       (assert (#{:number :char} (:node/kind keydecl))
                                         {:node/kind (:node/kind keydecl)})
                                       (if (= :char (:node/kind keydecl))
                                         (int (:value keydecl))
                                         (int (rv-data/parsed-number-value keydecl))))
                        intkeys (if (= :vector (:node/kind keydecl))
                                 (mapv keydecl->int (:children keydecl))
                                 [(keydecl->int keydecl)])
                        prev (or (peek (:cases acc)) test)]
                    (-> acc
                      (update :cases conj (ana/analyse-after prev then))
                      (update :keymap into
                        (mapv (fn [intkey] [intkey (count (:cases acc))]) intkeys)))))
          {:cases [] :keymap {}}
          (partitionv 2 (subvec children 2)))
        fallback (when (odd? (count children))
                   (ana/analyse-after test (peek children)))
        spec (join-branch-specs (cond-> cases
                                  fallback (conj fallback)))]
    (ana/transfer-branch-env test
      {:node/kind :case
       :mode :int
       :test test
       :keymap keymap
       :cases cases
       :fallback fallback
       :node/spec spec})))

(defn anasf-case-enum [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [test (ana/analyse-expr node (nth children 1))
        {:keys [keymap cases]}
        (reduce (fn [acc [enum then]]
                  (let [_ (assert (#{:symbol :keyword :vector} (:node/kind enum)))
                        enums (if (= :vector (:node/kind enum))
                                (mapv :string (:children enum))
                                [(:string enum)])
                        prev (or (peek (:cases acc)) test)]
                    (-> acc
                      (update :cases conj (ana/analyse-after prev then))
                      (update :keymap into
                        (mapv (fn [e] [e (count (:cases acc))]) enums)))))
          {:keymap {} :cases []}
          (partitionv 2 (subvec children 2)))
        fallback (when (odd? (count children))
                   (ana/analyse-after test (peek children)))
        enum-cls (spec/get-exact-class (:node/spec test))
        spec (join-branch-specs (cond-> cases
                                  fallback (conj fallback)))]
    (assert (some? enum-cls))
    (ana/transfer-branch-env test
      {:node/kind :case
       :mode :enum
       :classname enum-cls
       :test test
       :keymap keymap
       :cases cases
       :fallback fallback
       :node/spec spec})))

(defn anasf-when [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [test (ana/analyse-expr node (nth children 1))
        body (ana/analyse-body test (subvec children 2))
        bspec (:node/spec body)
        bprim (spec/prim? bspec)]
    (ana/transfer-branch-env body
      {:node/kind :if-true
       :node/spec (if (= :jump (:spec/kind bspec))
                    {:spec/kind :exact-class :classname "void"}
                    bspec)
       :test test :then body
       :else (if (= :jump(:spec/kind bspec))
               (ana/new-void-node)
               (cond
                 (= "boolean" bprim)
                 (ana/new-const-prim-node test false)
                 (= "void" bprim)
                 (ana/new-void-node)
                 bprim
                 (throw (ex-info "Unsupported when body prim" {"bprim" bprim}))
                 :else {:node/kind :nil}))})))

(defn anasf-not [{:keys [children] :as node}]
  ;; TODO could use bit-xor with ..00001
  (assert (= 2 (count children)))
  (let [child (ana/analyse-expr node (nth children 1))]
    (ana/transfer-branch-env child
      {:node/kind :if-true
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :test child
       :then (ana/new-const-prim-node child false)
       :else (ana/new-const-prim-node child true)})))

(defn anasf-nil? [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  (let [child (ana/analyse-expr node (nth children 1))]
    (ana/transfer-branch-env child
      {:node/kind :if-nil
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :test child
       :then (ana/new-const-prim-node child true)
       :else (ana/new-const-prim-node child false)})))

(defn anasf-instance? [{:keys [children node/env] :as node}]
  ;; java does not auto-cast in case of 'if (o instanceof T) {...}'
  ;; so neither will I
  (assert (= 3 (count children)))
  (let [classname (ana/expand-classname env (:string (nth children 1)))
        child (ana/analyse-expr node (nth children 2))]
    (ana/transfer-branch-env child
      {:node/kind :instance-of
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :classname classname
       :arg child})))

(defn anasf-array-set [{:keys [children] :as node}]
  (assert (<= 4 (count children)))
  (let [target (ana/analyse-expr node (nth children 1))
        index (ana/analyse-expr target (nth children 2))
        val (ana/analyse-expr index (nth children 3))]
    (ana/transfer-branch-env val
      {:node/kind :array-set
       :target target
       :index index
       :val val
       :node/spec {:spec/kind :exact-class :classname "void"}})))

(defn anasf-array-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [target (ana/analyse-expr node (nth children 1))
        index (ana/analyse-expr target (nth children 2))]
    (ana/transfer-branch-env index
      {:node/kind :array-get
       :target target
       :index index
       :node/spec (spec/get-array-element (:node/spec target))})))

(defn anasf-array-length [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  (let [target (ana/analyse-expr node (nth children 1))]
    (ana/transfer-branch-env target
      {:node/kind :array-length
       :target target
       :node/spec (spec/of-class "int")})))

(defn anasf-throw [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  (let [arg (first (ana/analyse-args node (subvec children 1)))]
    (ana/transfer-branch-env arg
      {:node/kind :throw
       :node/spec {:spec/kind :jump}
       :arg arg})))

(defn anasf-locking [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [[lock] (ana/analyse-args node (subvec children 1 2))
        body (ana/analyse-body lock (subvec children 2))]
    {:node/kind :locking
     :node/spec (:node/spec body)
     :lock lock
     :body body}))

(defn anasf-try [{:keys [children node/env] :as node}]
  (if (= 1 (count children))
    (ana/new-void-node)
    (let [list-of? (fn [s child]
                     (and (= :list (:node/kind child))
                       (when-some [f (first (:children child))]
                         (and (= :symbol (:node/kind f))
                           (= s (:string f))))))
          ntry (reduce (fn [acc child]
                         (if (or (list-of? "catch" child)
                               (list-of? "finally" child))
                           (reduced acc)
                           (inc acc)))
                 0 (subvec children 1))
          exceptable? (not= 0 ntry)
          try-body (ana/analyse-body node (subvec children 1 (inc ntry)))
          catch-prev try-body
          catches
          (reduce
            (fn [acc {:keys [children] :as child}]
              (if (list-of? "catch" child)
                (do (assert (<= 3 (count children)))
                  (let [cs (nth children 1)
                        sym (nth children 2)
                        _ (assert (= :symbol (:node/kind sym)))
                        symname (:string sym)
                        classnames
                        (condp = (:node/kind cs)
                          :symbol [(:string cs)]
                          :vector (mapv (fn [x]
                                          (assert (= :symbol (:node/kind x))
                                            "Invalid catch union")
                                          (:string x))
                                    (:children cs)))
                        classnames (mapv (partial ana/expand-classname env) classnames)
                        local-info (ana/make-local-info symname
                                     {:spec/kind :exact-class
                                      :classname (if (= 1 (count classnames))
                                                   (first classnames)
                                                   ;; TODO ideally common ancestor
                                                   "java.lang.Throwable")})]
                    (conj acc
                      {:classnames classnames
                       :local-id (:id local-info)
                       :body (ana/analyse-body
                               (assoc-in catch-prev [:node/locals symname]
                                 local-info)
                               (subvec children 3))})))
                (reduced acc)))
            [] (when exceptable? (subvec children (inc ntry))))
          nrest (- (count children) (count catches) ntry 1)
          _ (assert (<= nrest 1))
          finally-body (when (= 1 nrest)
                         (let [c (peek children)]
                           (if (list-of? "finally" c)
                             (ana/analyse-body
                               (assoc-in catch-prev [:node/env :restart-targets] nil)
                               (subvec (:children c) 1))
                             (throw (RuntimeException. "Bad final try-catch clause")))))]
      {:node/kind :try
       :node/spec (join-branch-specs
                    (conj (mapv :body catches) try-body finally-body))
       :try-body try-body
       :catches catches
       :finally-body finally-body
       :node/env (:node/env node)
       :node/locals (:node/locals node)})))

#_(defn anasf-while [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [[test] (ana/analyse-args node (subvec children 1 2))
        body (ana/analyse-body node (subvec children 2))]
    {:node/kind :while
     :node/spec {:spec/kind :void}
     :test test :body body}))

'(let-jump-targets ;; could calculate fall through
   [(body []
      (dosomething)
      (test))
    (test []
      (when condition
        (body)))]
   (test))
'(with-recur-targets ;; obvious fall-through
   _ (jump test)
   body (dosomething)
   test (when condition
          (jump body)))

;; also: like subroutine, but does not resume
;; tail pos / return / break / recur etc
'(let-rets [(proc [] ...)]
   (if x
     (do ...
       (proc))
     (proc)))

(defn analyse-loop-acc-assigns [prev-node pairs]
  (reduce (fn [decls [sym vexpr]]
            (let [prev (or (peek decls) prev-node)
                  init (ana/analyse-expr prev vexpr)
                  nam (:string sym)]
              (assert (string? nam))
              (conj decls
                (ana/transfer-branch-env init
                  {:node/kind :assign-local
                   :node/spec (spec/of-class "void")
                   :local-name nam
                   :val init
                   :statement? true
                   :node/locals
                   (assoc (:node/locals init)
                     nam (ana/make-local-info nam (:node/spec init)))}))))
    [] pairs))

(defn anasf-loop [{:keys [children] :as node}]
  ;; could have optional name, like function, and call like function
  (assert (<= 2 (count children)))
  (let [bindvec (nth children 1)
        _ (assert (= :vector (:node/kind bindvec)))
        pairs (partitionv 2 (:children bindvec))
        decls (analyse-loop-acc-assigns node pairs)
        id (random-uuid)
        prev (or (peek decls) node)
        local-map (:node/locals prev)
        prev' (update-in prev [:node/env :restart-targets]
               (fnil conj []) {:id id
                               :locals (mapv (comp (fn [name]
                                                     (assoc (get local-map name)
                                                       :name name))
                                               :string first)
                                         pairs)})
        body (ana/analyse-body prev' (subvec children 2))]
    (ana/transfer-branch-env body
      {:node/kind :do
       :children
       (conj decls {:node/kind :restart-target
                    :id id
                    :body body})
       :node/env (assoc (:node/env body) :restart-targets
                   (-> node :node/env :restart-targets))
       :node/spec (:node/spec body)})))

(defn anasf-loopr [{:keys [children node/env] :as node}]
  (assert (<= 4 (count children)))
  (let
    [coll-vec-ast (nth children 1)
     acc-vec-ast (nth children 2)
     body-ast (nth children 3)
     final-ast (nth children 4 nil)
     collinfos
     (loop-zip [[coll-sym-ast coll-init-ast]
                ^Iterable (partitionv 2 (:children coll-vec-ast))]
       [prev node
        locals (:node/locals node)
        collinfos []]
       (let [init-ana (ana/analyse-after (update prev :node/env ana/expression-env)
                        coll-init-ast)
             local-name (:string coll-sym-ast)
             local (ana/make-local-info local-name
                     ;; TODO figure out element type
                     (spec/of-class "java.lang.Object"))]
         (recur init-ana locals
           (conj collinfos
             {:coll-init init-ana
              :item-local-name local-name
              :item-local local})))
       collinfos)
     it-assigns
     (mapv (fn [{:keys [coll-init]}]
             (ana/transfer-branch-env coll-init
               {:node/kind :assign-local
                :node/spec (spec/of-class "void")
                :local-id (str (gensym "it_"))
                :val {:node/kind :jcall
                      :interface? true
                      :node/spec (spec/of-class "java.util.Iterator")
                      :target coll-init
                      :method-name "iterator"
                      :method-type
                      (Type/getMethodType "()Ljava/util/Iterator;")
                      :args []}
                :statement? true}))
       collinfos)
     it-item-assigns
     (loop-zip [[{:keys [item-local-name item-local]} it-assign]
                ^Iterable (map vector collinfos it-assigns)]
       [it-item-assigns []
        locals (:node/locals node)]
       (let [it-use {:node/kind :local-use
                     :node/spec (spec/of-class "java.util.Iterator")
                     :local-id (:local-id it-assign)}
             prev (or (peek it-item-assigns) node)
             locals (assoc locals item-local-name item-local)]
         (recur
           (conj it-item-assigns
             (ana/transfer-branch-env prev
               {:node/kind :assign-local
                :node/spec (spec/of-class "void")
                :node/locals locals
                :local-name item-local-name
                :val {:node/kind :jcall
                      :interface? true
                      :node/spec (spec/of-class "java.lang.Object")
                      :target it-use
                      :method-name "next"
                      :method-type
                      (Type/getMethodType "()Ljava/lang/Object;")
                      :args []}
                :statement? true}))
           locals))
       it-item-assigns)
     prev (or (peek it-item-assigns) node)
     it-item-checks
     (mapv (fn [{:keys []} it-assign]
             {:node/kind :jcall
              :interface? true
              :node/spec (spec/of-class "boolean")
              :target {:node/kind :local-use
                       :node/spec (spec/of-class "java.util.Iterator")
                       :local-id (:local-id it-assign)}
              :method-name "hasNext"
              :method-type
              (Type/getMethodType "()Z")
              :args []})
       collinfos it-assigns)
     acc-pairs (partitionv 2 (:children acc-vec-ast))
     acc-assigns (analyse-loop-acc-assigns prev acc-pairs)
     jump-id (random-uuid)
     prev (update-in (or (peek acc-assigns) prev)
            [:node/env :restart-targets]
            (fnil conj [])
            {:id jump-id
             :locals (mapv (fn [{:keys [local-name node/locals]}]
                             (assoc (get locals local-name)
                               :name local-name))
                       acc-assigns)})
     it-item-check-test
     (ana/transfer-branch-env prev
       (reduce (fn [check prev-check]
                 {:node/kind :if-true
                  :node/spec (spec/of-class "boolean")
                  :test prev-check
                  :then check
                  :else (ana/new-const-prim-node test false)})
         (peek it-item-checks)
         (reverse (pop it-item-checks))))
     env (:node/env it-item-check-test)
     loop-body-ana (ana/analyse-after prev body-ast)
     final-ana (if final-ast
                 (ana/analyse-after prev final-ast)
                 (ana/new-void-node prev))
     [[loop-body-ana final-ana] spec]
     (coerce-join-branches env [loop-body-ana final-ana])
     body-ana
     (ana/transfer-branch-env it-item-check-test
       {:node/kind :if-true
        :node/spec spec
        :test it-item-check-test
        :then {:node/kind :do
               :children (conj it-item-assigns loop-body-ana)}
        :else final-ana})]
    (ana/transfer-branch-env body-ana
      {:node/kind :do
       :node/spec (:node/spec body-ana)
       :node/env (assoc (:node/env body-ana) :restart-targets
                   (-> node :node/env :restart-targets))
       :children
       (into [] cat [it-assigns acc-assigns
                     [{:node/kind :restart-target
                       :id jump-id
                       :body body-ana}]])})))

(defn anasf-recur [{:keys [children] :as node}]
  (let [jump-target (peek (:restart-targets (:node/env node)))
        jump-id (:id jump-target)
        _ (when (nil? jump-id)
            (throw (ex-info
                     (str "Could not find recur target")
                     (select-keys (:node/env node) [:restart-targets]))))
        locals (:locals jump-target)
        recur-args (subvec children 1)
        _ (when (not= (count locals) (count recur-args))
            (throw (ex-info "Mismatched recur arg number"
                     {:locals (mapv :name locals)
                      :expected (count locals)
                      :got (count recur-args)})))
        assigns
        (reduce (fn [assigns [local vexpr]]
                  (let [prev (or (peek assigns) node)
                        init (ana/analyse-expr prev vexpr)
                        nam (:name local)]
                    (assert (string? nam) (str "local: " (pr-str local)))
                    (assert (some? (:id local)) {:keys (keys local)})
                    (conj assigns
                      (ana/transfer-branch-env init
                        {:node/kind :assign-local
                         :node/spec (spec/of-class "void")
                         :local-name nam
                         :local-id (:id local)
                         :val init
                         :statement? true
                         :node/locals
                         (assoc-in (:node/locals init)
                           [nam :spec] (:node/spec init))}))))
          [] (map vector locals recur-args))]
    (ana/transfer-branch-env (or (peek assigns) node)
      {:node/kind :do
       :node/spec {:spec/kind :jump}
       :children (conj assigns
                   {:node/kind :jump
                    :id jump-id})})))

(defn anasf-new-array [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [classname (:string (nth children 1))
        classname (ana/expand-classname (:node/env node) classname)
        dims (ana/analyse-args node (subvec children 2))
        ndims (count dims)]
    (ana/transfer-branch-env (peek dims)
      {:node/kind :new-array
       :node/spec {:spec/kind :exact-array
                   :classname classname
                   :ndims ndims}
       :classname classname
       :ndims ndims ;; TODO allow override with keyword option :dims n
       :dims dims})))

(defn get-method-pretypes [env target-class static? method-name nargs]
  (let [self-class (try (interop/resolve-class env (:self-classname env))
                     (catch ClassNotFoundException _ Object))]
    (interop/get-methods-pretypes self-class target-class static? method-name nargs)))

(defn determine-method [env methods0 args]
  {:post [(some? %) (some? (nth % 0))]}
  (let [arg-types (mapv (comp (partial spec/get-duck-class env) :node/spec) args)
        methods0 methods0]
    (loop-zip [method methods0] ;; first find exact match
      [first-noneqs []]
      (let [param-types (interop/-getParamTypes method)
            first-noneq
            (loop-zip [param-type ^Iterable (vec param-types)
                       i :idx]
              []
              (let [arg-type (nth arg-types i nil)]
                (if (and arg-type (interop/same-class? arg-type param-type))
                  (recur)
                  i))
              -1)]
        (if (<= 0 first-noneq)
          (recur (conj first-noneqs first-noneq))
          [method args]))
      ;; then try inheritance match
      (loop-zip [method ^Iterable methods0
                 first-noneq ^Iterable first-noneqs]
        [first-nomatches []
         some-match? false]
        (let [param-types (interop/-getParamTypes method)
              n-fixed-params (min (count param-types) (count arg-types))
              first-nomatch
              (if (< (count arg-types) (count param-types)) ;; varargs
                first-noneq
                (loop [i first-noneq]
                  (if (< i n-fixed-params)
                    (let [param-type (nth param-types i)
                          arg-type (nth arg-types i)
                          matches? (interop/-satisfies? arg-type param-type)]
                      (if matches?
                        (recur (inc i))
                        i))
                   -1)))]
          (recur
            (conj first-nomatches first-nomatch)
            (or some-match? (< first-nomatch 0))))
        (if some-match?
          ;; find most specific of inheritance-matching methods
          (loop-zip [fnm ^Iterable first-nomatches]
            [i 0 most-specific nil]
            (if (<= 0 fnm)
              (recur (inc i) most-specific)
              (let [m (nth methods0 i)
                    r (if (nil? most-specific)
                        m
                        (let [ms-params (interop/-getParamTypes most-specific)
                              m-params (interop/-getParamTypes m)]
                          (loop 
                            [i (dec (count m-params)) s1 false s2 false
                             diff? false]
                            (if (<= 0 i)
                              (let [p1 (nth ms-params i)
                                    p2 (nth m-params i)]
                                (recur (dec i)
                                  (or s1 (interop/-satisfies? p2 p1))
                                  (or s2 (interop/-satisfies? p1 p2))
                                  (or diff? (not (interop/same-class? p1 p2)))))
                              (if diff?
                                (if (and s1 s2)
                                  (throw (ex-info "method ambiguous (no casting)"
                                           {:arg-types arg-types
                                            :method-param-types
                                            (mapv (comp vec interop/-getParamTypes) methods0)}))
                                  (if s1 most-specific m))
                                ;; may be overriding implementations
                                ;; pick the earlier method (lowest in class hierarchy)
                                most-specific)))))]
                (recur (inc i) r)))
            [most-specific args])
          ;; else try casting
          (loop-zip [method ^Iterable (let [seen-params (java.util.HashSet.)]
                                        (filterv ;; pick earliest override
                                          (fn [m]
                                            (let [pt (vec (interop/-getParamTypes m))]
                                              (if (.contains seen-params pt)
                                                false
                                                (.add seen-params pt))))
                                          methods0))
                     first-nomatch ^Iterable first-nomatches]
            [m+args* []]
            (let
              [param-types (interop/-getParamTypes method)
               nparams (count param-types)
               ; max-i (dec nparams)
               varargs? (interop/-varargs? method)
               [args' spread?]
               (loop [i first-nomatch
                      nparams nparams
                      args' args
                      vartype nil]
                 (if (< i nparams)
                   (let [cto (or vartype (nth param-types i))
                         cfrom (nth arg-types i nil)]
                     (if (nil? cfrom)
                       (recur (inc i) nparams args' (interop/-elementType cto))
                       (let [coercion (ana/get-coercion env cfrom cto)]
                         (if (and (nil? coercion)
                               ;; FIXME duplicate work from inheritance matching
                               (not (interop/-satisfies? cfrom cto)))
                           (if (and varargs? (= i (dec nparams)) (nil? vartype))
                             (recur i (count arg-types) args'
                               (interop/-elementType cto))
                             nil)
                           (recur (inc i) nparams
                             (update args' i assoc :node/coercion coercion)
                             vartype)))))
                   [args' (some? vartype)]))]
              (recur
                (if args'
                  (conj m+args* [method args' spread?])
                  m+args*)))
            (case (count m+args*)
              0 (throw (ex-info "no match with casting"
                         {:arg-types arg-types
                          :method-param-types (mapv (comp vec interop/-getParamTypes) methods0)}))
              1 (nth m+args* 0)
              (throw (ex-info "method ambiguous with casting"
                       {:arg-types arg-types
                        :method-param-types (mapv (comp vec interop/-getParamTypes) methods0)})) ;; ambiguous
              )))))))

(defn get-ctor-pretypes [env target-class nargs]
  (let [self-class (try (interop/resolve-class env (:self-classname env))
                     (catch ClassNotFoundException _ Object))]
    (interop/get-ctor-pretypes self-class target-class nargs)))

(defn anasf-new-obj [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        classname (ana/expand-classname (:node/env node) classname)
        argsdecl (subvec children 2)
        args (ana/analyse-args node argsdecl)
        nargs (count args)
        final (or (last args) node)
        env (:node/env final)
        ; new-ctor (first (filter #(= nargs (count (:param-classnames %)))
        ;             (get-in node [:node/env :new-classes classname :constructors])))
        target-class (interop/resolve-class (:node/env node) classname)
        ; ctype (if new-ctor
        ;         (Type/getMethodType Type/VOID_TYPE
        ;           (into-array Type (map type/classname->type (:param-classnames new-ctor))))
        ;         (interop/match-ctor-type env classname
        ;           (mapv (comp spec/get-exact-class :node/spec) args)))
        ctors0 (get-ctor-pretypes env target-class (count argsdecl))
        [method args spread?] (determine-method env ctors0 args)
        ctype (interop/ctor->type method)
        ]
    (ana/transfer-branch-env final
      {:node/kind :new-obj
       :node/spec {:spec/kind :exact-class
                   :classname classname}
       :classname classname
       :spread spread?
       :method-type ctype
       :args args})))

(defn anasf-jcall* [{:keys [node/env] :as node}
                    target-ast method-classname method-name args-ast]
  (let [target (ana/analyse-expr node target-ast)
        c (or method-classname (spec/get-exact-class (:node/spec target)))
        _ (assert (some? c))
        target-class (interop/resolve-class (:node/env node) c)
        methods0 (get-method-pretypes env target-class false method-name (count args-ast))
        args (ana/analyse-args target args-ast)
        ; nargs (count args)
        ; new-method (first (filter #(= nargs (count (:param-classnames %)))
        ;                     (get-in node [:node/env :new-classes classname :instance-methods])))
        final (or (last args) target)
        env (:node/env final)
        [method args spread?] (determine-method env methods0 args)
        ; new-class (get-in node [:node/env :new-classes c])
        mt (interop/method->type method)
        ; mt (interop/lookup-method-type (:node/env final) c false method-name
        ;      (mapv (comp (partial spec/get-duck-class env) :node/spec) args) nil)
        tags (ana/get-meta-tags (:node/meta node))]
    (ana/transfer-branch-env final
      (cond->
        {:target target
         :method-name method-name
         :method-type mt
         :spread spread?
         :args args
         :dynamic (or (some #{"dynamic"} tags)
                    (interop/dynamic-class? (:node/env node) target-class))
         :node/spec (spec/of-class (type/get-classname (.getReturnType mt)))
         }
        method-classname
        (assoc :node/kind :jcall-specific
          :classname method-classname)
        (not method-classname)
        (assoc :node/kind :jcall
          :interface? (interop/-interface? target-class))))))

(defn anasf-jcall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [m (:string (nth children 2))
        _ (assert (string? m))]
    (anasf-jcall* node (nth children 1) nil m (subvec children 3))))

(defn anasf-jscall*
  [classname method-name {:keys [node/env jcall-dynamic] :as node} arg-asts]
  (let [_ (assert (string? method-name) "Invalid method-name argument")
        _ (assert (string? classname))
        c (ana/expand-classname env classname)
        target-class (interop/resolve-class env c)
        methods0 (get-method-pretypes env target-class true method-name (count arg-asts))
        _ (when-not (< 0 (count methods0))
            (throw (ex-info "no accessible methods matching name+arity"
                     {:name method-name :classname c})))
        args (ana/analyse-args node arg-asts)
        [method args spread?] (determine-method env methods0 args)
        final (or (last args) node)
        mt (interop/method->type method)]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :classname c
       :method-name method-name
       :method-type mt
       :spread spread?
       :dynamic (or jcall-dynamic
                  (interop/dynamic-class? (:node/env node) target-class))
       :node/spec (spec/of-class (type/get-classname (.getReturnType mt)))
       :args args})))

(defn anasf-jscall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [classname (:string (nth children 1))
        method-name (:string (nth children 2))
        ; tags (mapv :string (ana/get-meta-tags (:node/meta (nth children 0))))
        ]
    (anasf-jscall* classname method-name
      (cond-> node false;(some #{"dynamic"} tags)
        (assoc :jcall-dynamic true))
      (subvec children 3))))

(defn anasf-jfield-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [obj (nth children 1)
        _ (assert (= :symbol (:node/kind obj)))
        f (nth children 2)
        _ (assert (or (= :symbol (:node/kind f))
                    (= :keyword (:node/kind f))))
        field (:string f)
        ; target (ana/analyse-expr node obj)
        ; c (spec/get-exact-class (:node/spec target))
        c (ana/expand-classname (:node/env node) (:string obj))
        ft (interop/lookup-field-type (:node/env node) c true field)]
    (ana/transfer-branch-env node ;target
      {:node/kind :get-field
       ; :target target
       :classname c
       :field-name field
       :field-type ft
       :node/spec (spec/of-class (type/get-classname ft))})))

(defn anasf-jifield-get* [node target-ast field]
  (let [target (ana/analyse-expr node target-ast)
        c (spec/get-exact-class (:node/spec target))
        target-class (interop/resolve-class (:node/env node) c)
        _ (when (nil? c) (throw (RuntimeException. "Could not resolve type for jfi")))
        ft (interop/lookup-field-type (:node/env target) c false field)]
    (ana/transfer-branch-env target
      {:node/kind :get-field
       :target target
       :classname c
       :field-name field
       :field-type ft
       :dynamic (interop/dynamic-class? (:node/env node) target-class)
       :node/spec (spec/of-class (type/get-classname ft))})))

(defn anasf-jifield-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (anasf-jifield-get* node (nth children 1)
    (let [f (nth children 2)
          _ (assert (= :keyword (:node/kind f)))
          field (:string f)]
      field)))

(defn anasf-prefix-jcall [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let [mast (nth children 0)
        m (:string mast)]
    (assert (string? m))
    (if (= \: (nth m 1))
      (let [fieldname (subs m 2)]
        (anasf-jifield-get* node (nth children 1) fieldname))
      (let [m (subs m 1)
            mc (defclass/class-tag (assoc mast :node/env env))]
        (anasf-jcall* node (nth children 1) mc m (subvec children 2))))))

(defn anasf-set-field [{:keys [children node/env] :as node}]
  (when-not (= 4 (count children))
    (throw (RuntimeException. "setf! must have three args")))
  (let [targetdecl (nth children 1)
        target (ana/analyse-expr node targetdecl)
        target-classname (spec/get-exact-class (:node/spec target))
        target-class (interop/resolve-class env target-classname)
        fdecl (nth children 2)
        _ (assert (= :keyword (:node/kind fdecl)))
        field-name (:string fdecl)
        ft (interop/lookup-field-type env target-class false field-name)
        ; _ (assert (:mutable field)
        ;     (str "field " field-name " not declared mutable"))
        valdecl (nth children 3)
        v (ana/analyse-expr target valdecl)
        statement? (ana/in-statement? env)]
    (ana/transfer-branch-env v
      {:node/kind :set-field
       :target target
       :classname target-classname
       :field-name field-name
       :field-type ft
       :val v
       :statement? statement?
       :node/spec (if statement?
                    (spec/of-class "void")
                    (:node/spec v))})))

(defn anasf-cast [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [cdecl (nth children 1)
        classname (if (= :symbol (:node/kind cdecl))
                    (:string cdecl)
                    (:value cdecl))
        _ (assert (string? classname))
        classname (ana/expand-classname (:node/env node) classname)
        t (type/classname->type classname)
        x (ana/analyse-after node (nth children 2))
        env (:node/env node)
        to-prim? (type/prim? t)
        from-prim (when-some [p (spec/prim? (:node/spec x))]
                    (keyword p))
        obj->prim? (and to-prim? (not from-prim))
        prim->obj? (and from-prim (not to-prim?))
        to-type (if obj->prim?
                  (type/box t) t)
        coercion
        (when (or obj->prim? prim->obj?)
          (let [box (if obj->prim?
                      (type/box t)
                      (type/box (type/prim-classname->type (name from-prim))))
                boxname (type/get-classname box)
                co (ana/get-coercion env
                     (if prim->obj?
                       (spec/get-duck-class env (:node/spec x))
                       (interop/resolve-class env boxname))
                     (interop/resolve-class env (if prim->obj? boxname classname)))]
            (or co
              (throw (ex-info "No coercion" {:to classname :from (:node/spec x)})))))
        spec (spec/of-class classname)]
    (if prim->obj?
      (assoc x
        :node/coercion coercion
        :node/spec spec)
      (ana/transfer-branch-env x
        {:node/kind :cast
         :type to-type
         :from-prim from-prim
         :body x
         :node/coercion coercion
         :node/spec spec}))))

(defn anasf-<- [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (ana/analyse-after node
    (reduce (fn [acc node]
              (assert (#{:vector :list} (:node/kind node)))
              (update node :children conj acc))
      (reverse (drop 1 children)))))

(defn anasf--> [{:keys [children] :as node}]
  (assert (<= 1 (count children)))
  (ana/analyse-after node
    (reduce (fn [node' {:as acc :keys [children]}]
              (let [acc (if (#{:vector :list} (:node/kind acc))
                          acc
                          {:node/kind :list
                           :children [acc]})]
                (assoc acc :children
                  (into [(nth children 0) node']
                    (subvec children 1)))))
      (drop 1 children))))

; (swap! ana/*sf-analysers assoc "not" #'anasf-not)
(swap! ana/*sf-analysers assoc "=" #'anasf-eq)
(swap! ana/*sf-analysers assoc "==" #'anasf-identical?)
(swap! ana/*sf-analysers assoc ">=" #'anasf-gte)
(swap! ana/*sf-analysers assoc ">" #'anasf-gt)
(swap! ana/*sf-analysers assoc "<=" #'anasf-lte)
(swap! ana/*sf-analysers assoc "<" #'anasf-lt)
(swap! ana/*sf-analysers assoc "if" #'anasf-if)
(swap! ana/*sf-analysers assoc "and" #'anasf-and)
(swap! ana/*sf-analysers assoc "or" #'anasf-or)
(swap! ana/*sf-analysers assoc "not" #'anasf-not)
(swap! ana/*sf-analysers assoc "nil?" #'anasf-nil?)
(swap! ana/*sf-analysers assoc "instance?" #'anasf-instance?)
(swap! ana/*sf-analysers assoc "nw" #'anasf-new-obj)
(swap! ana/*sf-analysers assoc "na" #'anasf-new-array)
(swap! ana/*sf-analysers assoc "aa" #'anasf-array-get)
(swap! ana/*sf-analysers assoc "sa" #'anasf-array-set)
(swap! ana/*sf-analysers assoc "alength" #'anasf-array-length)
(swap! ana/*sf-analysers assoc "throw" #'anasf-throw)
(swap! ana/*sf-analysers assoc "locking" #'anasf-locking)
(swap! ana/*sf-analysers assoc "ji" #'anasf-jcall)
(swap! ana/*sf-analysers assoc "jc" #'anasf-jscall)
(swap! ana/*sf-analysers assoc "jf" #'anasf-jfield-get)
(swap! ana/*sf-analysers assoc "jfi" #'anasf-jifield-get)
(swap! ana/*sf-analysers assoc "setf!" #'anasf-set-field)
(swap! ana/*sf-analysers assoc "loop" #'anasf-loop)
(swap! ana/*sf-analysers assoc "loopr" #'anasf-loopr)
(swap! ana/*sf-analysers assoc "recur" #'anasf-recur)
(swap! ana/*sf-analysers assoc "when" #'anasf-when)
(swap! ana/*sf-analysers assoc "ct" #'anasf-cast)
(swap! ana/*sf-analysers assoc "try" #'anasf-try)
(swap! ana/*sf-analysers assoc "defclass" #'anasf-defclass)
(swap! ana/*sf-analysers assoc "case-enum" #'anasf-case-enum)
(swap! ana/*sf-analysers assoc "case" #'anasf-case-int)
(swap! ana/*sf-analysers assoc "<-" #'anasf-<-)
(swap! ana/*sf-analysers assoc "->" #'anasf-->)

;; TODO use comptime info to skip conditional jumps
'(loop [x y]
  (if (nil? x)
    A
    (do ...
      ;; goto A
      (recur nil))))







