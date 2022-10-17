(ns jl.compiler.sforms
  (:require
    [chic.util :refer [inherit-vars loopr loop-zip]]
    [jl.interop :as interop]
    [jl.compiler.defclass :as defclass]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node]]
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
        _ (assert (and prim1? prim2?)
            (str "types " clsname1 " " clsname2 " not primitive"))]
    (if (= clsname1 clsname2)
      [x1 x2 (keyword clsname1)]
      (let [cls1 (interop/resolve-class env clsname1)
            cls2 (interop/resolve-class env clsname2)
            c1 (ana/get-coercion env cls1 cls2)]
        (if c1
          [(assoc x1 :node/coercion c1) x2 (keyword clsname2)]
          (let [c2 (ana/get-coercion env cls2 cls1)]
            (if c2
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

;; TODO detect mergeable ifs (if (if t t (do 3 f)) 1 2) -> (if t 1 (do 3 2))
(defn anasf-if [{:keys [children] :as node}]
  (assert (= 4 (count children)))
  (let [test (ana/analyse-expr node (nth children 1))
        then (ana/analyse-after test (nth children 2))
        else (ana/analyse-after test (nth children 3))
        s (join-branch-specs [then else])]
    ; (throw (RuntimeException.
    ;          (str "Unmatching if specs:\nif: "
    ;            (pr-str sthen) "\nelse: " (pr-str selse))))
    {:node/kind :if-true
     :node/spec s
     :test test
     :then then
     :else else
     :node/env (:node/env test)
     :node/locals (:node/locals test)}))

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
  (let [test (ana/analyse-expr node (nth children 1))
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
    {:node/kind :if-true
     :node/spec (if (= :jump (:spec/kind bspec))
                  {:spec/kind :exact-class :classname "void"}
                  bspec)
     :test test :then body
     :else (if (= :jump(:spec/kind bspec))
             (ana/new-void-node)
             (cond (= "boolean" bprim)
                  (ana/new-const-prim-node test false)
                  (= "void" bprim)
                  (ana/new-void-node)
                  bprim (throw (RuntimeException. "Unsupported when prim"))
                  :else {:node/kind :nil}))
     :node/env (:node/env test)
     :node/locals (:node/locals test)}))

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

(defn anasf-loop [{:keys [children] :as node}]
  ;; could have optional name, like function, and call like function
  (assert (<= 2 (count children)))
  (let [bindvec (nth children 1)
        _ (assert (= :vector (:node/kind bindvec)))
        pairs (partitionv 2 (:children bindvec))
        decls (reduce (fn [decls [sym vexpr]]
                        (let [prev (or (peek decls) node)
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
                [] pairs)
        id (random-uuid)
        prev (or (peek decls) node)
        prev' (update-in prev [:node/env :restart-targets]
               (fnil conj []) {:id id
                               :locals (mapv (comp (fn [n] {:name n}) :string first) pairs)})
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
                    (conj assigns
                      (ana/transfer-branch-env init
                        {:node/kind :assign-local
                         :node/spec (spec/of-class "void")
                         :local-name nam
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

(defn anasf-new-obj [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        classname (ana/expand-classname (:node/env node) classname)
        args (ana/analyse-args node (subvec children 2))
        nargs (count args)
        final (or (last args) node)
        env (:node/env final)
        new-ctor (first (filter #(= nargs (count (:param-classnames %)))
                    (get-in node [:node/env :new-classes classname :constructors])))
        ctype (if new-ctor
                (Type/getMethodType Type/VOID_TYPE
                  (into-array Type (map type/classname->type (:param-classnames new-ctor))))
                (interop/match-ctor-type env classname
                  (mapv (comp spec/get-exact-class :node/spec) args)))]
    (ana/transfer-branch-env final
      {:node/kind :new-obj
       :node/spec {:spec/kind :exact-class
                   :classname classname}
       :classname classname
       :method-type ctype
       :args args})))

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
              (let [arg-type (nth arg-types i)]
                (if (interop/same-class? arg-type param-type)
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
              first-nomatch
              (loop [i first-noneq]
                (if (< i (count param-types))
                  (let [param-type (nth param-types i)
                        arg-type (nth arg-types i)
                        matches? (interop/-satisfies? arg-type param-type)]
                    (if matches?
                      (recur (inc i))
                      i))
                  -1))]
          (recur (conj first-nomatches first-nomatch)
            (or some-match? (< first-nomatch 0))))
        (if some-match?
          ;; find most specific of inheritance-matching methods
          (loop-zip [fnm ^Iterable first-nomatches]
            [i 0 most-specific nil]
            (if (< 0 fnm)
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
               args'
               (loop [i first-nomatch
                      args' args]
                 (if (< i (count param-types))
                   (let [cto (nth param-types i)
                         cfrom (nth arg-types i)
                         coercion (ana/get-coercion env cfrom cto)]
                     (if (and (nil? coercion)
                           ;; FIXME duplicate work from inheritance matching
                           (not (interop/-satisfies? cfrom cto)))
                       nil
                       (recur (inc i)
                         (update args' i assoc :node/coercion coercion))))
                   args'))]
              (recur
                (if args'
                  (conj m+args* [method args'])
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
        [method args] (determine-method env methods0 args)
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

(defn anasf-prefix-jcall [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let [mast (nth children 0)
        m (:string mast)
        _ (assert (string? m))
        m (subs m 1)
        mc (defclass/class-tag (assoc mast :node/env env))]
    (anasf-jcall* node (nth children 1) mc m (subvec children 2))))

(defn anasf-jscall [{:keys [children node/env] :as node}]
  (assert (<= 3 (count children)))
  (let [c (:string (nth children 1))
        method-name (:string (nth children 2))
        _ (assert (string? method-name) "Invalid method-name argument")
        _ (assert (string? c))
        c (ana/expand-classname (:node/env node) c)
        target-class (interop/resolve-class (:node/env node) c)
        argsdecl (subvec children 3)
        methods0 (get-method-pretypes env target-class true method-name (count argsdecl))
        _ (assert (< 0 (count methods0)) "no accessible methods matching name+arity")
        args (ana/analyse-args node argsdecl)
        [method args] (determine-method env methods0 args)
        final (or (last args) node)
        mt (interop/method->type method)
        tags (mapv :string (ana/get-meta-tags (:node/meta (nth children 0))))]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :classname c
       :method-name method-name
       :method-type mt
       :dynamic (or (some #{"dynamic"} tags)
                  (interop/dynamic-class? (:node/env node) target-class))
       :node/spec (spec/of-class (type/get-classname (.getReturnType mt)))
       :args args})))

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

(defn anasf-jifield-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [obj (nth children 1)
        f (nth children 2)
        _ (assert (= :keyword (:node/kind f)))
        field (:string f)
        target (ana/analyse-expr node obj)
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
        x (ana/analyse-after node (nth children 2))]
    (ana/transfer-branch-env x
      {:node/kind :cast
       :type t
       :from-prim
       (when (type/prim? t)
         (if-some [p (spec/prim? (:node/spec x))]
           (keyword p)
           (throw (RuntimeException. "Cannot cast non-prim to prim"))))
       :body x
       :node/spec (spec/of-class classname)})))

(defn anasf-<- [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (ana/analyse-after node
    (reduce (fn [acc node]
              (assert (#{:vector :list}(:node/kind node)))
              (update node :children conj acc))
      (reverse (next children)))))

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
(swap! ana/*sf-analysers assoc "recur" #'anasf-recur)
(swap! ana/*sf-analysers assoc "when" #'anasf-when)
(swap! ana/*sf-analysers assoc "ct" #'anasf-cast)
(swap! ana/*sf-analysers assoc "try" #'anasf-try)
(swap! ana/*sf-analysers assoc "defclass" #'anasf-defclass)
(swap! ana/*sf-analysers assoc "case-enum" #'anasf-case-enum)
(swap! ana/*sf-analysers assoc "case" #'anasf-case-int)
(swap! ana/*sf-analysers assoc "<-" #'anasf-<-)

;; TODO use comptime info to skip conditional jumps
'(loop [x y]
  (if (nil? x)
    A
    (do ...
      ;; goto A
      (recur nil))))







