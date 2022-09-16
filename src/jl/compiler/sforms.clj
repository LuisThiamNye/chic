(ns jl.compiler.sforms
  (:require
    [chic.util :refer [inherit-vars]]
    [jl.interop :as interop]
    [jl.compiler.defclass :as defclass]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node]]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)))

(inherit-vars defclass/anasf-defclass)

#_(defn anasf-isnil [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    {:node/kind :unary-cmp
     }))

;; TODO integer optimisations eg (< -1 i) => (<= 0 i) - compare to zero
(defn anasf-numcmp [op {:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))
        clsname1 (spec/get-exact-class (:node/spec x1))
        clsname2 (spec/get-exact-class (:node/spec x2))
        prim1? (type/prim-classname->type clsname1)
        prim2? (type/prim-classname->type clsname2)
        prim? prim1?
        _ (assert (= prim1? prim2?)
            (str "types " clsname1 " " clsname2 " not equal"))
        clsname clsname1]
    (ana/transfer-branch-env x2
      {:node/kind :num-cmp-2
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :op op
       :type (if prim?
               (keyword clsname) :ref)
       :arg1 x1 :arg2 x2})))
(defn anasf-gte [node] (anasf-numcmp :>= node))
(defn anasf-gt [node] (anasf-numcmp :> node))
(defn anasf-lte [node] (anasf-numcmp :<= node))
(defn anasf-lt [node] (anasf-numcmp :< node))
(defn anasf-identical? [node] (anasf-numcmp := node))
(defn anasf-eq [node] (anasf-numcmp := node))

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

(defn anasf-case-enum [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [test (ana/analyse-expr node (nth children 1))
        cases
        (reduce (fn [acc [enum then]]
                  ;; TODO in future support multiple cases for enum, as vector
                  (let [_ (assert (#{:symbol :keyword} (:node/kind enum)))
                        enum (:string enum)
                        prev (or (peek (peek acc)) test)]
                    (conj acc [enum (ana/analyse-after prev then)])))
          [] (partitionv 2 (subvec children 2)))
        fallback (when (odd? (count children))
                   (ana/analyse-after test (peek children)))
        enum-cls (spec/get-exact-class (:node/spec test))
        spec (join-branch-specs (cond-> (mapv peek cases)
                                  fallback (conj fallback)))]
    (assert (some? enum-cls))
    (ana/transfer-branch-env test
      {:node/kind :case
       :mode :enum
       :classname enum-cls
       :test test
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
                                                 (:string x))
                                           (:children cs)))
                                classnames (mapv (partial ana/expand-classname env) classnames)]
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
                               (assoc-in (:node/locals init)
                                 [nam :spec] (:node/spec init))}))))
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
            (throw (RuntimeException.
                     (str "Could not find recur target"
                       "\nEnv: " (:node/env node)))))
        locals (:locals jump-target)
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
          [] (map vector locals (subvec children 1)))]
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

(defn anasf-jcall* [{:keys [node/env] :as node}
                    target-ast method-classname method-name args-ast]
  (let [target (ana/analyse-expr node target-ast)
        c (or method-classname (spec/get-exact-class (:node/spec target)))
        args (ana/analyse-args target args-ast)
        ; nargs (count args)
        ; new-method (first (filter #(= nargs (count (:param-classnames %)))
        ;                     (get-in node [:node/env :new-classes classname :instance-methods])))
        new-class (get-in node [:node/env :new-classes c])
        target-class (interop/resolve-class (:node/env node) c)
        final (or (last args) target)
        mt (interop/lookup-method-type (:node/env final) c false method-name
             (mapv (comp (partial spec/get-duck-class env) :node/spec) args) nil)
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

(defn anasf-jscall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [c (:string (nth children 1))
        m (:string (nth children 2))
        _ (assert (string? m) "Invalid method-name argument")
        _ (assert (string? c))
        c (ana/expand-classname (:node/env node) c)
        target-class (interop/resolve-class (:node/env node) c)
        args (ana/analyse-args node (subvec children 3))
        final (or (last args) node)
        mt (interop/lookup-method-type (:node/env final) c true m
             (mapv (comp (partial spec/get-duck-class (:node/env node)) :node/spec) args) nil)
        tags (mapv :string (ana/get-meta-tags (:node/meta (nth children 0))))]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :classname c
       :method-name m
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
(swap! ana/*sf-analysers assoc "loop" #'anasf-loop)
(swap! ana/*sf-analysers assoc "recur" #'anasf-recur)
(swap! ana/*sf-analysers assoc "when" #'anasf-when)
(swap! ana/*sf-analysers assoc "ct" #'anasf-cast)
(swap! ana/*sf-analysers assoc "try" #'anasf-try)
(swap! ana/*sf-analysers assoc "defclass" #'anasf-defclass)
(swap! ana/*sf-analysers assoc "case-enum" #'anasf-case-enum)
(swap! ana/*sf-analysers assoc "<-" #'anasf-<-)

;; TODO use comptime info to skip conditional jumps
'(loop [x y]
  (if (nil? x)
    A
    (do ...
      ;; goto A
      (recur nil))))







