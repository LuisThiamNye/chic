(ns jl.compiler.analyser
  (:require
    [jl.compiler.type :as type]
    [chic.util :refer [<-]]
    [jl.rv-data :as rv-data]
    [jl.compiler.spec :as spec]
    [jl.reader :as reader :refer [PFormVisitor]]
    [chic.debug :as debug]))

(defn new-void-node []
  {:node/kind :void
   :node/spec {:spec/kind :exact-class
               :classname "void"}})

(defn transfer-branch-env [prev node]
  (conj {:node/locals (:node/locals prev)
         :node/env (:node/env prev)}
    node))

(declare -analyse-node)

(defn analyse-after [prev node]
  (let [locals (:node/locals prev)]
    (-analyse-node
      (cond-> (assoc node :node/env (:node/env prev))
        locals (assoc :node/locals locals)))))

(defn analyse-args [prev args]
  (if (= 0 (count args))
    []
    (let [n1 (analyse-after prev (nth args 0))]
      (if (= 1 (count args))
        [n1]
        (reduce
          (fn [acc node]
            (conj acc (analyse-after (peek acc) node)))
          [n1] (subvec args 1))))))

(defn analyse-body [prev body]
  (if (= 0 (count body))
    (new-void-node)
    (let [n1 (analyse-after prev (nth body 0))]
      (if (= 1 (count body))
        n1
        (let [children
              (reduce
                (fn [acc node]
                  (conj acc (analyse-after (peek acc) node)))
                [n1] (subvec body 1))
              tail (peek children)
              locals (:node/locals tail)]
          (cond-> {:node/kind :do
                   :children children
                   :node/spec (:node/spec tail)}
            locals (assoc :node/locals locals)))))))

(defn new-const-prim-node [prev value]
  (transfer-branch-env prev
    {:node/kind :const-prim
     :node/spec {:spec/kind :exact-class
                 :classname (.getClassName
                              (type/unbox(type/obj-classname->type
                                           (.getName (class value)))))}
     :value value}))

(defn analyse-number [node]
  (let [v (rv-data/parsed-number-value node)]
    (new-const-prim-node node v))
  #_(cond-> (assoc node :node/kind :number)
    ; (= :none (:type node))
    ; (assoc :node/spec
    ;   {:spec/kind :traits
    ;    :traits [(if (= :int (:kind node))
    ;               :integer :number)]})
    (not= :none (:type node))
    (do )
    #_(assoc :node/spec
      (spec/specialise-num-literal (:type node) nil))))

(defn analyse-string [node]
  (assoc node :node/spec {:spec/kind :exact-class
                          :classname "java.lang.String"}))

(defn analyse-char [node]
  (assoc node :node/spec {:spec/kind :exact-class
                          :classname "char"}))

(defn analyse-keyword [node]
  (assoc node :node/spec {:spec/kind :exact-class
                          :classname "clojure.lang.Keyword"}))

(defn anasf-quote [{:keys [children] :as node}]
  (when (< 2 (count children))
    (throw (RuntimeException. "quote only supports one child"))
    (/ 0)))

(defn anasf-assign [{:keys [children]:as node}]
  (when-not (= 3 (count children))
    (throw (RuntimeException. "set! must have two args")))
  (let [target (nth children 1)]
    (or (when (= :symbol (:node/kind target))
          (let [sym (:string target)]
            (when-some [local (get (:node/locals node) sym)]
              (let [v (analyse-after node (nth children 2))
                    s (:node/spec v)]
                  {:node/kind :assign-local
                   :node/spec {:spec/kind :exact-class :classname "void"}
                   :local-name sym
                   :val v
                   :node/env (:node/env v)
                   :node/locals
                   (cond-> (:node/locals v)
                     (nil? (:spec local))
                     (assoc :node/locals
                       (assoc-in (:node/locals node) [sym :spec] s)))}))))
      (throw (RuntimeException. "Could not resolve set! target")))))
#_
(defn anasf-with-locals [{:keys [children]:as node}]
  (case (count children)
    1 (throw (RuntimeException. "Invalid with-locals form"))
    (let [symvec (nth children 1)
          _ (when-not (= :vector (:node/kind symvec))
              (throw (RuntimeException. "Invalid local vec")))
          locals (reduce
                   (fn [l sn]
                     (if (= :symbol (:node/kind sn))
                       (assoc l (:string sn) {})
                       (throw (RuntimeException. "Invalid local declaration"))))
                   (:node/locals node)
                   (:children symvec))]
      (analyse-body {:node/locals locals} (subvec children 2)))))

(defn anasf-introduce-local [{:keys [children] :as node}]
  (assert (<= 3 (count children)))
  (let [nam (:string (nth children 1))
        _ (assert (string? nam))
        init (analyse-after node (nth children 2))]
    {:node/kind :assign-local
     :node/spec {:spec/kind :exact-class :classname "void"}
     :local-name nam
     :val init
     :node/env (:node/env init)
     :node/locals (assoc-in (:node/locals init) [nam :spec]
                    (:node/spec init))}))

(defn anasf-do [{:keys [children] :as node}]
  (analyse-body node (subvec children 1)))

(def *sf-analysers
  (atom {"quote" #'anasf-quote
         ; "with-locals" #'anasf-with-locals
         "set!" #'anasf-assign
         "do" #'anasf-do
         "l=" #'anasf-introduce-local}))
(comment (swap! *sf-analysers assoc "l=" #'anasf-introduce-local))

(defn resolve-sf [nam]
  (@*sf-analysers nam))

(defn analyse-symbol [{:keys [string] :as node}]
  (or (when (:invoke-target? node)
        (when-some [sf (resolve-sf string)]
          (assoc node :special-form sf)))
    (when-some [local (get (:node/locals node) string)]
      (transfer-branch-env node
        {:node/kind :local-use
         :node/spec (:spec local)
         :local-name string}))
    (condp = string
      "nil" (transfer-branch-env node {:node/kind :nil})
      "true" (new-const-prim-node node true)
      "false" (new-const-prim-node node false)
      nil)
    (throw (RuntimeException. (str "Could not resolve symbol: " string
                                "\nLocals: " (pr-str (:node/locals node)))))))

(defn analyse-list [{:keys [children] :as node}]
  (if (= 0 (count children))
    (throw (RuntimeException. "Unquoted empty list not allowed"))
    (let [c1 (-analyse-node (assoc (nth children 0) :invoke-target? true))
          sf (:special-form c1)]
      (if sf
        (sf node)
        (throw (RuntimeException. "cannot invoke"))))))

(defn analyse-vector [node])

(defn analyse-set [node])

(defn analyse-map [node])

(defn -analyse-node [node]
  ((condp = (:node/kind node)
     :symbol analyse-symbol
     :list analyse-list
     :keyword analyse-keyword
     :vector analyse-vector
     :map analyse-map
     :number analyse-number
     :string analyse-string
     :set analyse-set
     :char analyse-char
     (throw (RuntimeException. (str "AST node analyser not found: " node))))
   node))

(deftype BaseAnaRdrVisitor [parent node cb]
  PFormVisitor
  (-visitNumber [_ numstr]
    (let [num (rv-data/parse-number numstr)]
      (if (nil? num)
        (throw (RuntimeException. (str "Invalid number: " numstr)))
        (rv-data/-addEnd cb
          (assoc num :node/kind :number)))))
  (-visitKeyword [_ kwstr]
    (rv-data/-addEnd cb
      {:node/kind :keyword
       :string (subs kwstr 1)}))
  (-visitSymbol [_ symstr]
    (rv-data/-addEnd cb
      {:node/kind :symbol
       :string symstr}))
  (-visitChar [_ token]
    (rv-data/-addEnd cb 
      {:node/kind :char
       :value (rv-data/interpret-char-token token)}))
  (-visitCharLiteral [_ c]
    (rv-data/-addEnd cb
      {:node/kind :char
       :value (char c)}))
  (-visitString [_ s]
    (rv-data/-addEnd cb
      {:node/kind :string
       :value s}))
  (-visitList [self] 
    (BaseAnaRdrVisitor. self {:node/kind :list} (rv-data/clj-vector-builder)))
  (-visitVector [self]
    (BaseAnaRdrVisitor. self {:node/kind :vector} (rv-data/clj-vector-builder)))
  (-visitMap [self]
    (BaseAnaRdrVisitor. self {:node/kind :map} (rv-data/clj-map-builder)))
  (-visitSet [self]
    (BaseAnaRdrVisitor. self {:node/kind :set} (rv-data/clj-set-builder)))
  (-visitDiscard [_] (reader/noop-visitor))
  (-visitEnd [_]
    (when parent
      (rv-data/-addEnd (.-cb ^BaseAnaRdrVisitor parent)
        (assoc node :children (rv-data/-toColl cb)))))
  rv-data/PCollBuilder
  (-toColl [_] (rv-data/-toColl cb)))

(defn base-rdrvisitor []
  (->BaseAnaRdrVisitor nil nil (rv-data/clj-vector-builder)))
  
(defn str->ast [s]
  (let [v (base-rdrvisitor)]
    (reader/read-all-forms v
      (reader/str-reader-default s)
      #_(reader/file-reader-default "src2/okcolour.clj"))
    (rv-data/-toColl v)))

(comment
  
  (-> (str->ast "(+ 1 2)")
    (-analyse-node))
  
  (-> (str->ast "
(with-locals [x] (set! x 1i))")
    #_(clojure.pprint/pprint)
    first
    (-analyse-node)
    clojure.pprint/pprint
    )
  
  (-> (str->ast "
nil")
    first
    (-analyse-node)
    clojure.pprint/pprint

  )