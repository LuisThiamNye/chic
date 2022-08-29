(ns jl.compiler.sforms
  (:require
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [clojure.core.match :refer [match]]
    [jl.compiler.analyser :as ana :refer [-analyse-node]])
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

(defn anasf-numcmp [op {:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [[x1 x2] (ana/analyse-args node (subvec children 1))]
    (ana/transfer-branch-env x2
      {:node/kind :num-cmp-2
       :node/spec {:spec/kind :exact-class :classname "boolean"}
       :op op
       :type :int
       :arg1 x1 :arg2 x2})))
(defn anasf-gte [node] (anasf-numcmp :>= node))
(defn anasf-gt [node] (anasf-numcmp :> node))
(defn anasf-lte [node] (anasf-numcmp :<= node))
(defn anasf-lt [node] (anasf-numcmp :< node))

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
                     (str"Unmatching if specs: " sthen "\n" selse))))]
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

(defn anasf-new-obj [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        args (ana/analyse-args node (subvec children 2))
        final (or (last args) node)]
    (ana/transfer-branch-env final
      {:node/kind :new-obj
       :node/spec {:spec/kind :exact-class
                   :classname classname}
       :classname classname
       :args args})))

(defn anasf-new-array [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [classname (:string (nth children 1))
        dims (ana/analyse-args node (subvec children 2))]
    (ana/transfer-branch-env (peek dims)
      {:node/kind :new-array
       :node/spec {:spec/kind :exact-class
                   :classname classname}
       :classname classname
       :ndims (count dims) ;; TODO override with keyword option :dims n
       :dims dims})))

(defn anasf-throw [{:keys [children] :as node}]
  (assert (= 2 (count children)))
  {:node/kind :throw
   :arg (first (ana/analyse-args node (subvec children 1)))})

(defn anasf-locking [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [[lock] (ana/analyse-args node (subvec children 1 2))
        body (ana/analyse-body lock (subvec children 2))]
    {:node/kind :locking
     :node/spec (:node/spec body)
     :lock lock
     :body body}))

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
                          {:node/kind :assign-local
                           :node/spec {:spec/kind :exact-class :classname "void"}
                           :local-name nam
                           :val init
                           :node/locals
                           (assoc-in (:node/locals init)
                             [nam :spec] (:node/spec init))})))
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
                      {:node/kind :assign-local
                       :node/spec {:spec/kind :exact-class :classname "void"}
                       :local-name nam
                       :val init
                       :node/locals
                       (assoc-in (:node/locals init)
                         [nam :spec] (:node/spec init))})))
          [] (map vector locals (subvec children 1)))]
    (ana/transfer-branch-env {};(or (peek assigns) node)
      {:node/kind :do
       :node/spec {:spec/kind :jump}
       :children (conj assigns
                   {:node/kind :jump
                    :id jump-id})})))

(defn anasf-jcall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [target (ana/analyse-after node (nth children 1))
        m (:string (nth children 2))
        _ (assert (string? m))
        c (spec/get-exact-class (:node/spec target))
        args (ana/analyse-args target (subvec children 3))
        mt (interop/match-method-type c false m
             (mapv (comp spec/get-exact-class :node/spec) args) nil)
        final (or (last args) node)]
    (ana/transfer-branch-env final
      {:node/kind :jcall
       :target target
       :method-name m
       :method-type mt
       :node/spec {:spec/kind :exact-class
                   :classname (.getClassName (.getReturnType mt))}
       ; :desc (interop/match-method-desc c m
       ;         (mapv (comp spec/get-exact-class :node/spec) args) nil)
       :args args})))

(defn anasf-jscall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [c (:string (nth children 1))
        m (:string (nth children 2))
        _ (assert (string? m))
        _ (assert (string? c))
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

(defn anasf-cast [{:keys [children] :as node}]
  (assert (= 3 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        x (ana/analyse-after node (nth children 2))]
    (ana/transfer-branch-env x
      {:node/kind :cast
       :classname classname
       :body x
       :node/spec {:spec/kind :exact-class
                   :classname classname}})))

; (swap! ana/*sf-analysers assoc "not" #'anasf-not)
(swap! ana/*sf-analysers assoc "=" #'anasf-eq)
(swap! ana/*sf-analysers assoc ">=" #'anasf-gte)
(swap! ana/*sf-analysers assoc ">" #'anasf-gt)
(swap! ana/*sf-analysers assoc "<=" #'anasf-lte)
(swap! ana/*sf-analysers assoc "<" #'anasf-lt)
(swap! ana/*sf-analysers assoc "if" #'anasf-if)
(swap! ana/*sf-analysers assoc "nw" #'anasf-new-obj)
(swap! ana/*sf-analysers assoc "na" #'anasf-new-array)
(swap! ana/*sf-analysers assoc "tw" #'anasf-throw)
(swap! ana/*sf-analysers assoc "locking" #'anasf-locking)
(swap! ana/*sf-analysers assoc "ji" #'anasf-jcall)
(swap! ana/*sf-analysers assoc "jc" #'anasf-jscall)
(swap! ana/*sf-analysers assoc "loop" #'anasf-loop)
(swap! ana/*sf-analysers assoc "recur" #'anasf-recur)
(swap! ana/*sf-analysers assoc "when" #'anasf-when)
(swap! ana/*sf-analysers assoc "cast" #'anasf-cast)








