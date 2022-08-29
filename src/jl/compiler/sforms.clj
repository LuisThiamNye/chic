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
    {:node/kind :num-cmp-2
     :node/spec {:spec/kind :exact-class :classname "boolean"}
     :op op
     :type :int
     :arg1 x1 :arg2 x2
     :node/locals (:node/locals x2)}))
(defn anasf-gte [node] (anasf-numcmp :>= node))
(defn anasf-gt [node] (anasf-numcmp :> node))
(defn anasf-lte [node] (anasf-numcmp :<= node))
(defn anasf-lt [node] (anasf-numcmp :< node))

;; TODO detect mergeable ifs (if (if t t (do 3 f)) 1 2) -> (if t 1 (do 3 2))
(defn anasf-if [{:keys [children] :as node}]
  (assert (= 4 (count children)))
  (let [[test] (ana/analyse-args node (subvec children 1 2))
        [then] (ana/analyse-args test (subvec children 2 3))
        [else] (ana/analyse-args test (subvec children 3 4))]
    {:node/kind :if-true
     :node/spec {:spec/kind :exact-class :classname "int"}
     :test test
     :then then
     :else else
     :node/locals (:node/locals test)}))

(defn anasf-when [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [[test] (ana/analyse-args node (subvec children 1 2))
        body (ana/analyse-body test (subvec children 2))]
    {:node/kind :if-true
     :node/spec (:node/spec body)
     :test test :then body
     :else {:spec/kind :exact-class :classname "void"}
     :node/locals (:node/locals test)}))

(defn anasf-new-obj [{:keys [children] :as node}]
  (assert (<= 2 (count children)))
  (let [classname (:string (nth children 1))
        _ (assert (string? classname))
        args (ana/analyse-args node (subvec children 2))]
    {:node/kind :new-obj
     :node/spec {:spec/kind :exact-class
                 :classname classname}
     :classname classname
     :args args
     :node/locals (:node/locals (or (last args) node))}))

(defn anasf-new-array [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [classname (:string (nth children 1))
        dims (ana/analyse-args node (subvec children 2))]
    {:node/kind :new-array
     :node/spec {:spec/kind :exact-class
                 :classname classname}
     :classname classname
     :ndims (count dims) ;; TODO override with keyword option :dims n
     :dims dims}))

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
                            [init] (ana/analyse-args prev [vexpr])
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
        body (ana/analyse-body (or (peek decls) node) (subvec children 2))]
    {:node/kind :do
     :children
     (conj decls {:node/kind :restart-target
                  :id (random-uuid)
                  :body (conj decls body)})
     :node/spec (:node/spec body)
     :node/locals (:node/locals body)}))

(defn anasf-recur [{:keys [children] :as node}]
  (let [jump-id (:id (peek (:restart-targets (:node/env node))))
        _ (assert (some? jump-id))
        assigns
        (reduce (fn [assigns [local vexpr]]
                  (let [prev (or (peek assigns) node)
                        [init] (ana/analyse-args prev [vexpr])
                        nam (:local-name local)]
                    (assert (string? nam))
                    (conj assigns
                      {:node/kind :assign-local
                       :node/spec {:spec/kind :exact-class :classname "void"}
                       :local-name nam
                       :val init
                       :node/locals
                       (assoc-in (:node/locals init)
                         [nam :spec] (:node/spec init))})))
          [] (map vector (subvec children 1)))]
    {:node/kind :do
     :children (conj assigns
                 {:node/kind :jump
                  :id jump-id})
     :node/locals (:node/locals (or (peek assigns) node))}))

(defn anasf-jcall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [[target] (ana/analyse-args node (subvec children 1 2))
        m (:string (nth children 2))
        _ (assert (string? m))
        c (spec/get-exact-class (:node/spec target))
        args (ana/analyse-args target (subvec children 3))
        mt (interop/match-method-type c false m
             (mapv (comp spec/get-exact-class :node/spec) args) nil)]
    {:node/kind :jcall
     :target target
     :method-name m
     :node/spec {:spec/kind :exact-class
                 :classname (.getClassName (.getReturnType mt))}
     ; :desc (interop/match-method-desc c m
     ;         (mapv (comp spec/get-exact-class :node/spec) args) nil)
     :args args
     :node/locals (:node/locals (or (last args) node))}))

(defn anasf-jscall [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [c (:string (nth children 1))
        m (:string (nth children 2))
        _ (assert (string? m))
        _ (assert (string? c))
        args (ana/analyse-args node (subvec children 3))
        mt (interop/match-method-type c true m
             (mapv (comp spec/get-exact-class :node/spec) args) nil)]
    {:node/kind :jcall
     :classname c
     :method-name m
     :node/spec {:spec/kind :exact-class
                 :classname (.getClassName (.getReturnType mt))}
     ; :desc (interop/match-method-desc c m
     ;         (mapv (comp spec/get-exact-class :node/spec) args) nil)
     :args args
     :node/locals (:node/locals (or (last args) node))}))

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
(swap! ana/*sf-analysers assoc "when" #'anasf-when)








