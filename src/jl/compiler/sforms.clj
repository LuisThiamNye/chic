(ns jl.compiler.sforms
  (:require
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node]]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)))



#_(defn anasf-isnil [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    {:node/kind :unary-cmp
     }))

(defn anasf-eq [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    {:node/kind :num-cmp-2
     :node/spec {:spec/kind :exact-class :classname "boolean"}
     :op :=
     :type :int
     :arg1 x1 :arg2 x2}))

;; TODO integer optimisations eg (< -1 i) => (<= 0 i) - compare to zero
(defn anasf-numcmp [op {:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))
        clsname (or (spec/get-exact-class (:node/spec x1))
                  (spec/get-exact-class (:node/spec x2)))]
    (ana/transfer-branch-env x2
      {:node/kind :num-cmp-2
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :op op
       :type (if (type/prim-classname->type clsname)
               (keyword clsname) :ref)
       :arg1 x1 :arg2 x2})))
(defn anasf-gte [node] (anasf-numcmp :>= node))
(defn anasf-gt [node] (anasf-numcmp :> node))
(defn anasf-lte [node] (anasf-numcmp :<= node))
(defn anasf-lt [node] (anasf-numcmp :< node))
(defn anasf-identical? [node] (anasf-numcmp := node))

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
  (let [test (ana/analyse-after node (nth children 1))
        then (ana/analyse-after test (nth children 2))
        else (ana/analyse-after test (nth children 3))
        sthen (:node/spec then)
        selse (:node/spec else)
        s (cond
            (= :jump (:spec/kind sthen))
            selse
            (= :jump (:spec/kind selse))
            sthen
            (= selse sthen)
            sthen
            :else
            (throw (RuntimeException.
                     (str "Unmatching if specs: " sthen "\n" selse))))]
    {:node/kind :if-true
     :node/spec s
     :test test
     :then then
     :else else
     :node/env (:node/env test)
     :node/locals (:node/locals test)}))

(defn anasf-when [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [test (ana/analyse-after node (nth children 1))
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
  (assert (= 2 (count children)))
  (let [child (ana/analyse-after node (nth children 1))]
    (ana/transfer-branch-env child
      {:node/kind :if-true
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :test child
       :then (ana/new-const-prim-node child false)
       :else (ana/new-const-prim-node child true)})))

(defn anasf-nil? [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  (let [child (ana/analyse-after node (nth children 1))]
    (ana/transfer-branch-env child
      {:node/kind :if-nil
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :test child
       :then (ana/new-const-prim-node child true)
       :else (ana/new-const-prim-node child false)})))

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

(defn anasf-array-set [{:keys [children] :as node}]
  (assert (<= 4 (count children)))
  (let [target (ana/analyse-after node (nth children 1))
        index (ana/analyse-after target (nth children 2))
        val (ana/analyse-after index (nth children 3))]
    (ana/transfer-branch-env val
      {:node/kind :array-set
       :target target
       :index index
       :val val
       :node/spec {:spec/kind :exact-class :classname "void"}})))

(defn anasf-array-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [target (ana/analyse-after node (nth children 1))
        index (ana/analyse-after target (nth children 2))]
    (ana/transfer-branch-env index
      {:node/kind :array-get
       :target target
       :index index
       :node/spec (spec/get-array-element (:node/spec target))})))

(defn anasf-throw [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  {:node/kind :throw
   :node/spec {:spec/kind :jump}
   :arg (first (ana/analyse-args node (subvec children 1)))})

(defn anasf-locking [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [[lock] (ana/analyse-args node (subvec children 1 2))
        body (ana/analyse-body lock (subvec children 2))]
    {:node/kind :locking
     :node/spec (:node/spec body)
     :lock lock
     :body body}))

(defn anasf-try [{:keys [children] :as node}]
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
          catches (reduce
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
                                                 (:string x)) (:children cs)))]
                            (conj acc
                              {:classnames classnames
                               :local-name symname
                               :body (ana/analyse-body
                                       (assoc-in catch-prev [:node/locals symname :spec]
                                         {:spec/kind :exact-class
                                          :classname (if (= 1 (count classnames))
                                                       (first classnames)
                                                       ;; TODO ideally common ancestor
                                                       "java.lang.Throwable")})
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
                              init (ana/analyse-after prev vexpr)
                              nam (:string sym)]
                          (assert (string? nam))
                          (conj decls
                            (ana/transfer-branch-env init
                              {:node/kind :assign-local
                               :node/spec {:spec/kind :exact-class :classname "void"}
                               :local-name nam
                               :val init
                               :node/locals
                               (assoc-in (:node/locals init)
                                 [nam :spec] (:node/spec init))}))))
                [] pairs)
        id (random-uuid)
        prev (or (peek decls) node)
        prev' (update-in prev [:node/env :restart-targets]
               (fnil conj []) {:id id
                               :locals (mapv (comp (fn [n] {:name n}) :string first) pairs)})
        body (ana/analyse-body prev' (subvec children 2))]
      {:node/kind :do
       :children
       (conj decls {:node/kind :restart-target
                    :id id
                    :body body})
       :node/env (assoc (:node/env body) :restart-targets
                   (-> node :node/env :restart-targets))
       :node/locals (:node/locals body)
       :node/spec (:node/spec body)}))

(defn anasf-recur [{:keys [children] :as node}]
  (let [jump-target (peek (:restart-targets (:node/env node)))
        jump-id (:id jump-target)
        _ (when (nil? jump-id)
            (throw (RuntimeException.
                     (str "Could not find recur target"
                       "\nEnv: " (:node/env node)))))
        locals (:locals jump-target)
        assigns
        (reduce (fn [assigns [local vexpr]]
                  (let [prev (or (peek assigns) node)
                        init (ana/analyse-after prev vexpr)
                        nam (:name local)]
                    (assert (string? nam) (str "local: " (pr-str local)))
                    (conj assigns
                      (ana/transfer-branch-env init
                        {:node/kind :assign-local
                         :node/spec {:spec/kind :exact-class :classname "void"}
                         :local-name nam
                         :val init
                         :node/locals
                         (assoc-in (:node/locals init)
                           [nam :spec] (:node/spec init))}))))
          [] (map vector locals (subvec children 1)))]
    (ana/transfer-branch-env (or (peek assigns) node)
      {:node/kind :do
       :node/spec {:spec/kind :jump}
       :children (conj assigns
                   {:node/kind :jump
                    :id jump-id})})))

(defn anasf-new-obj [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        classname (ana/expand-classname (:node/env node) classname)
        args (ana/analyse-args node (subvec children 2))
        nargs (count args)
        final (or (last args) node)
        new-ctor (first (filter #(= nargs (count (:param-classnames %)))
                    (get-in node [:node/env :new-classes classname :constructors])))
        ctype (if new-ctor
                (Type/getMethodType Type/VOID_TYPE
                  (into-array Type (map type/classname->type (:param-classnames new-ctor))))
                (interop/match-ctor-type classname
                  (mapv (comp spec/get-exact-class :node/spec) args)))]
    (ana/transfer-branch-env final
      {:node/kind :new-obj
       :node/spec {:spec/kind :exact-class
                   :classname classname}
       :classname classname
       :method-type ctype
       :args args})))

(defn anasf-jcall* [node target-ast method-name args-ast]
  (let [target (ana/analyse-after node target-ast)
        c (spec/get-exact-class (:node/spec target))
        args (ana/analyse-args target args-ast)
        ; nargs (count args)
        ; new-method (first (filter #(= nargs (count (:param-classnames %)))
        ;                     (get-in node [:node/env :new-classes classname :instance-methods])))
        new-class (get-in node [:node/env :new-classes c])
        mt (interop/match-method-type c false method-name
             (mapv (comp spec/get-exact-class :node/spec) args) nil)
        final (or (last args) target)]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :target target
       :interface? (if new-class
                     (contains? (:flags c) :interface)
                     (.isInterface (Class/forName c)))
       :method-name method-name
       :method-type mt
       :node/spec {:spec/kind :exact-class
                   :classname (.getClassName (.getReturnType mt))}
       ; :desc (interop/match-method-desc c m
       ;         (mapv (comp spec/get-exact-class :node/spec) args) nil)
       :args args})))

(defn anasf-jcall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [m (:string (nth children 2))
        _ (assert (string? m))]
    (anasf-jcall* node (nth children 1) m (subvec children 3))))

(defn anasf-prefix-jcall [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [m (:string (nth children 0))
        _ (assert (string? m))
        m (subs m 1)]
    (anasf-jcall* node (nth children 1) m (subvec children 2))))

(defn anasf-jscall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [c (:string (nth children 1))
        m (:string (nth children 2))
        _ (assert (string? m))
        _ (assert (string? c))
        c (ana/expand-classname (:node/env node) c)
        args (ana/analyse-args node (subvec children 3))
        mt (interop/match-method-type c true m
             (mapv (comp spec/get-exact-class :node/spec) args) nil)
        final (or (last args) node)]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :classname c
       :method-name m
       :method-type mt
       :node/spec {:spec/kind :exact-class
                   :classname (.getClassName (.getReturnType mt))}
       ; :desc (interop/match-method-desc c m
       ;         (mapv (comp spec/get-exact-class :node/spec) args) nil)
       :args args})))

(defn anasf-jfield-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [obj (nth children 1)
        _ (assert (= :symbol (:node/kind obj)))
        f (nth children 2)
        _ (assert (or (= :symbol (:node/kind f))
                    (= :keyword (:node/kind f))))
        field (:string f)
        ; target (ana/analyse-after node obj)
        ; c (spec/get-exact-class (:node/spec target))
        c (ana/expand-classname (:node/env node) (:string obj))
        ft (interop/lookup-field-type (:node/env node) c true field)]
    (ana/transfer-branch-env node ;target
      {:node/kind :get-field
       ; :target target
       :classname c
       :field-name field
       :type ft
       :node/spec {:spec/kind :exact-class
                   :classname (.getClassName ft)}})))

(defn anasf-jifield-get [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [obj (nth children 1)
        f (nth children 2)
        _ (assert (= :keyword (:node/kind f)))
        field (:string f)
        target (ana/analyse-after node obj)
        c (spec/get-exact-class (:node/spec target))
        _ (when (nil? c) (throw (RuntimeException. "Could not resolve type for jfi")))
        ft (interop/lookup-field-type (:node/env target) c false field)]
    (ana/transfer-branch-env target
      {:node/kind :get-field
       :target target
       :classname c
       :field-name field
       :type ft
       :node/spec {:spec/kind :exact-class
                   :classname (.getClassName ft)}})))

(defn anasf-cast [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [classname (:string (nth children 1))
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
       :node/spec {:spec/kind :exact-class
                   :classname classname}})))

(defn anasf-defclass [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [clsexpr (nth children 1)
        _ (assert (= :symbol (:node/kind clsexpr)))
        clsname (:string clsexpr)
        iface-parser (fn [opts {:keys [node/kind] :as x}]
                       (when (= :symbol kind)
                         (update opts :interfaces (fnil conj [])
                           (ana/expand-classname (:node/env node) (:string x)))))
        super-parser (fn [opts {:keys [node/kind string]}]
                       (if (= :symbol kind)
                         (assoc (dissoc opts :parser)
                           :super string)
                         (throw (UnsupportedOperationException.))))
        tag-parser
        (fn [opts {:keys [node/kind children]}]
          (if (= :vector kind)
            (assoc (dissoc opts :parser)
              :flags
              (vec (reduce
                     (fn [acc kw]
                       (let []
                         (when (acc kw)
                           (throw (IllegalArgumentException. "duplicate tag")))
                         (cond-> (conj acc kw)
                           (= :interface kw)
                           (recur :abstract))))
                     #{} (map (fn [c]
                                (assert (= :keyword (:node/kind c)))
                                (keyword (:string c)))
                           children))))
            (throw (UnsupportedOperationException.))))
        get-tags (fn [n]
                   (let [tag (:tag (:node/meta n))]
                      (when (= :vector (:node/kind tag))
                        (:children tag))))
        class-tag (fn [n]
                    (let [tag (:tag (:node/meta n))]
                      (when (= :vector (:node/kind tag))
                        (let [s(some #(when (= :symbol (:node/kind %))
                                        (:string %))
                                 (:children tag))]
                          (if (= "Self" s) clsname
                            (ana/expand-classname (:node/env node) s))))))
        parse-fieldvec (fn [opts x]
                         (let [flds (reduce (fn [fields f]
                                              (if (= :symbol (:node/kind f))
                                                (conj fields
                                                  {:name (:string f)
                                                   :flags #{:private :final}
                                                   :classname
                                                   (or (class-tag f)
                                                     "java.lang.Object")})
                                                (throw (UnsupportedOperationException.))))
                                      [] (:children x))]
                           (-> opts (assoc :fields flds)
                             (update :constructors (fnil conj [])
                               {:param-classnames (mapv :classname flds)
                                :auto true}))))
        adapt-env (fn [clsinfo env1]
                    (-> env1
                      (assoc :self-classname clsname)
                      (assoc-in [:new-classes clsname] clsinfo)
                      (assoc-in [:class-aliases "Self"] clsname)))
        parse-cfield
        (fn [opts {:keys [children]}]
          (assert (<= 3 (count children)))
          (let [namedecl (nth children 1)
                _ (assert (= :symbol (:node/kind namedecl)))
                tags (get-tags namedecl)
                name (:string namedecl)
                flaginfo (reduce (fn [acc t]
                                   (let [k (keyword (:string t))]
                                     (condp = k
                                       :priv (assoc acc :pub :private)
                                       :pub (assoc acc :pub :public)
                                       :pub-pkg (assoc acc :pub nil)
                                       (update acc conj :flags k))))
                           {:flags #{}}
                           (filter (fn [{:keys [node/kind]}]
                                     (= :keyword kind))
                             tags))
                flaginfo (conj {:pub :public
                                :mut :final} flaginfo)
                flags (cond-> (:flags flaginfo)
                        (:mut flaginfo)
                        (conj (:mut flaginfo)))
                flags (cond-> flags
                        (:pub flaginfo)
                        (conj (:pub flaginfo)))
                initdecl (nth children 2)
                init (ana/analyse-after (update (:prev-node opts) :node/env
                                          (partial adapt-env opts)) initdecl)
                fcls (or (spec/get-exact-class (:node/spec init))
                       "java.lang.Object")]
            (-> opts
              (assoc :prev-node
                (assoc-in init [:node/env :special-locals name]
                  {:fn (fn [pn]
                         (ana/transfer-branch-env pn
                           {:node/kind :get-field
                            :classname clsname
                            :field-name name
                            :type (type/classname->type fcls)
                            :node/spec {:spec/kind :exact-class
                                        :classname fcls}}))}))
              (update :class-fields (fnil conj [])
                {:name name
                 :flags flags
                 :val init
                 :classname fcls}))))
        parse-amethod
        (fn [opts {:keys [children]}]
          (assert (<= 2 (count children)))
          (let [mexp (nth children 1)
                _ (assert (= :symbol (:node/kind mexp)))
                mn (:string mexp)
                params (:children (nth children 2))
                ret-classname (or (class-tag params) "void")]
            (update opts :instance-methods conj
              {:name mn
               :flags #{:abstract :public} ;; either pub or priv must be set
               :ret-classname ret-classname
               :param-names (mapv :string params)
               :param-classnames
               (mapv (fn [p]
                       (class-tag p))
                 (drop 1 params))})))
        analyse-method-body
        (fn [prev-node clsinfo static? mn paramdecl bodydecl retc]
          (let [params (:children paramdecl)
                body (ana/analyse-body
                       {:node/env (adapt-env clsinfo (:node/env prev-node))
                        :node/locals
                        (into {}
                          (map-indexed
                            (fn [i {:keys [string] :as arg-node}]
                              (assert (= :symbol (:node/kind arg-node)))
                              [string {:arg-idx i
                                       :spec (when-some [cn (class-tag arg-node)]
                                               {:spec/kind :exact-class
                                                :classname cn})}]))
                          params)}
                       bodydecl)
                ret-classname (or retc (spec/get-exact-class (:node/spec body))
                                "void")]
            {:name mn
             :body body
             :flags #{:public}
             :ret-classname ret-classname
             :param-names (mapv :string params)
             :param-classnames
             (mapv (fn [p]
                     (let [c (class-tag p)]
                       (assert (some? c))
                       c))
               (if static? params (drop 1 params)))}))
        parse-named-cmethod
        (fn [opts mn paramdecl bodydecl retc]
          (update opts :class-methods conj
            (analyse-method-body (:prev-node opts) opts true mn paramdecl bodydecl retc)))
        parse-cmethod
        (fn [opts {:keys [children]}]
          (assert (<= 2 (count children)))
          (let [mexp (nth children 1)
                _ (assert (= :symbol (:node/kind mexp)))
                mn (:string mexp)
                paramdecl (nth children 2)]
            (parse-named-cmethod opts mn paramdecl (subvec children 3) nil)))
        parse-named-imethod
        (fn [opts mn paramdecl bodydecl retc]
          (let [fieldmap (into {}
                           (map (fn [{:keys [name classname]}]
                                  [name {:spec {:spec/kind :exact-class
                                                :classname classname}}]))
                           (:fields opts))]
            (update opts :instance-methods conj
            (analyse-method-body
              (assoc-in (:prev-node opts) [:node/env :fields] fieldmap)
              opts false mn paramdecl bodydecl retc))))
        parse-imethod
        (fn [opts {:keys [children]}]
          (assert (<= 2 (count children)))
          (let [mexp (nth children 1)
                _ (assert (= :symbol (:node/kind mexp)))
                mn (:string mexp)
                paramdecl (nth children 2)]
            (parse-named-imethod opts mn paramdecl (subvec children 3) nil)))
        parse-uninstall-method
        (fn [opts {:keys [children]}]
          (assert (<= 2 (count children)))
          (let [params (nth children 1)
                paramvec (:children params)
                _ (assert (and (= :vector (:node/kind params))
                    (empty? paramvec)))]
            (parse-named-cmethod opts "\uDB80\uDC00uninstall"
              params (subvec children 2) "void")))
        parse-decl (fn [opts {:keys [children] :as dnode}]
                     (assert (<= 2 (count children)))
                     (comp/define-stub-class opts)
                     (let [decl (nth children 0)
                           _ (assert (= :symbol (:node/kind decl)))
                           dname (:string decl)]
                       (condp = dname
                         "defi" (parse-imethod opts dnode)
                         "defn" (parse-cmethod opts dnode)
                         "defabstract" (parse-amethod opts dnode)
                         "def" (parse-cfield opts dnode)
                         "uninstall" (parse-uninstall-method opts dnode))))
        opts
        (reduce
          (fn [opts {:keys [node/kind] :as x}]
            (or (when-some [p (:parser opts)]
                  (p opts x))
              (let [opts (dissoc opts :parser)]
                (condp = kind
                  :keyword (let [p (condp = (:string x)
                                     "interfaces" iface-parser
                                     "super" super-parser
                                     "tag" tag-parser)]
                             (if (nil? p)
                               (throw (UnsupportedOperationException.))
                               (assoc opts :parser p)))
                  :vector (parse-fieldvec opts x)
                  :list (parse-decl opts x)))))
          {:classname clsname
           :prev-node node} (subvec children 2))
        ; {:keys [prev-node]} opts
        opts (dissoc opts :prev-node)]
    (assoc (ana/new-void-node)
      :node/locals (:node/locals node)
      :node/env (assoc-in (:node/env node) [:new-classes clsname] opts))))

; (swap! ana/*sf-analysers assoc "not" #'anasf-not)
(swap! ana/*sf-analysers assoc "=" #'anasf-eq)
(swap! ana/*sf-analysers assoc "==" #'anasf-identical?)
(swap! ana/*sf-analysers assoc ">=" #'anasf-gte)
(swap! ana/*sf-analysers assoc ">" #'anasf-gt)
(swap! ana/*sf-analysers assoc "<=" #'anasf-lte)
(swap! ana/*sf-analysers assoc "<" #'anasf-lt)
(swap! ana/*sf-analysers assoc "if" #'anasf-if)
(swap! ana/*sf-analysers assoc "not" #'anasf-not)
(swap! ana/*sf-analysers assoc "nil?" #'anasf-nil?)
(swap! ana/*sf-analysers assoc "nw" #'anasf-new-obj)
(swap! ana/*sf-analysers assoc "na" #'anasf-new-array)
(swap! ana/*sf-analysers assoc "aa" #'anasf-array-get)
(swap! ana/*sf-analysers assoc "sa" #'anasf-array-set)
(swap! ana/*sf-analysers assoc "throw" #'anasf-throw)
(swap! ana/*sf-analysers assoc "locking" #'anasf-locking)
(swap! ana/*sf-analysers assoc "ji" #'anasf-jcall)
(swap! ana/*sf-analysers assoc "jc" #'anasf-jscall)
(swap! ana/*sf-analysers assoc "jf" #'anasf-jfield-get)
(swap! ana/*sf-analysers assoc "jfi" #'anasf-jifield-get)
(swap! ana/*sf-analysers assoc "loop" #'anasf-loop)
(swap! ana/*sf-analysers assoc "recur" #'anasf-recur)
(swap! ana/*sf-analysers assoc "when" #'anasf-when)
(swap! ana/*sf-analysers assoc "ct" #'anasf-cast)
(swap! ana/*sf-analysers assoc "try" #'anasf-try)
(swap! ana/*sf-analysers assoc "defclass" #'anasf-defclass)

;; TODO use comptime info to skip conditional jumps
'(loop [x y]
  (if (nil? x)
    A
    (do ...
      ;; goto A
      (recur nil))))







