(ns jl.compiler.analyser
  (:require
    [jl.compiler.type :as type]
    [chic.util :refer [<- deftype+]]
    [jl.rv-data :as rv-data]
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.reader :as reader :refer [PFormVisitor]]
    [chic.debug :as debug]
    [jl.interop :as interop])
  (:import
    (org.objectweb.asm Type)))

(defn new-void-node []
  {:node/kind :void
   :node/spec {:spec/kind :exact-class
               :classname "void"}})

(defn get-meta-tags [m]
  (let [tag (:tag m)]
    (when (= :vector (:node/kind tag))
      (:children tag))))

(defn transfer-branch-env [prev node]
  (assert (some? (:node/env prev))
    (assoc (select-keys prev [:node/kind]) :keys (keys prev)))
  (conj {:node/locals (:node/locals prev)
         :node/env (:node/env prev)}
    node))

(declare -analyse-node)

(defn analyse-after [prev node]
  (assert (some? (:node/env prev)))
  (let [locals (:node/locals prev)
        ana (-analyse-node
              (cond-> (assoc node :node/env (:node/env prev))
                locals (assoc :node/locals locals)))]
    (assert (some? (:node/env ana)) (select-keys ana [:node/kind]))
    ana))

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
          (transfer-branch-env tail
            {:node/kind :do
             :children children
             :node/spec (:node/spec tail)}))))))

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
                          :classname "sq.lang.Keyword"}))

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

(defn anasf-let [{:keys [children] :as node}]
  (if (= 1 (count children))
    (transfer-branch-env node (new-void-node))
    (let [pairs (vec (partitionv 2 (subvec children 1)))
          tail (when (even? (count children))
                 (peek children))
          [pairs tail] (if tail [pairs tail]
                         [(pop pairs) (peek (peek pairs))])
          assigns (reduce
                    (fn [acc [sym val]]
                      (assert (= :symbol (:node/kind sym)))
                      (let [prev (or (peek acc) node)
                            aval (analyse-after prev val)
                            nam (:string sym)]
                        (conj acc
                          (transfer-branch-env aval
                            {:node/kind :assign-local
                             :local-name nam
                             :val aval
                             :node/spec {:spec/kind :exact-class :classname "void"}
                             :node/locals (assoc-in (:node/locals aval) [nam :spec]
                                            (:node/spec aval))}))))
                    [] pairs)
          tail (analyse-after (or (peek assigns) node) tail)]
      (transfer-branch-env tail
        {:node/kind :do
         :children (conj assigns tail)
         :node/spec (:node/spec tail)}))))

(defn anasf-do [{:keys [children] :as node}]
  (analyse-body node (subvec children 1)))

(def *sf-analysers
  (atom {"quote" #'anasf-quote
         ; "with-locals" #'anasf-with-locals
         "set!" #'anasf-assign
         "do" #'anasf-do
         "l=" #'anasf-introduce-local
         "=:" #'anasf-introduce-local
         "let" #'anasf-let}))
(comment (swap! *sf-analysers assoc "let" #'anasf-let)
  (swap! *sf-analysers dissoc "jfi'"))

(defn resolve-sf [nam]
  (@*sf-analysers nam))

(defn analyse-symbol [{:keys [^String string] :as node}]
  (assert (some? (:node/env node)))
  (or (when (:invoke-target? node)
        (or (when-some [sf (resolve-sf string)]
              (assoc node :special-form sf))
          (when (.startsWith string ".")
            (assoc node :special-form
              (requiring-resolve 'jl.compiler.sforms/anasf-prefix-jcall)))))
    (condp = string
      "nil" (transfer-branch-env node {:node/kind :nil
                                       :node/spec {:spec/kind :nil}})
      "true" (new-const-prim-node node true)
      "false" (new-const-prim-node node false)
      nil)
    (when-some [local (get (:node/locals node) string)]
      (transfer-branch-env node
        {:node/kind :local-use
         :node/spec (:spec local)
         :local-name string}))
    (when-some [local (get (:external-locals (:node/env node)) string)]
      (transfer-branch-env (update-in node [:node/env :captured-locals]
                             (fnil conj #{}) string)
        {:node/kind :self-field-get
         :field-name (str "%" string)
         :node/spec (:spec local)}))
    (when-some [field (get-in node [:node/env :fields string])]
      (transfer-branch-env node
        {:node/kind :self-field-get
         :node/spec (:spec field)
         :field-name string}))
    (when-some [local (get-in node [:node/env :special-locals string])]
      ((:fn local) node))
    (throw (RuntimeException.
             (str "Could not resolve symbol: " string
               "\nLocals: " (pr-str (:node/locals node))
               "\nFields: " (pr-str (:fields (:node/env node)))
               "\nSpecials: " (pr-str (:special-locals (:node/env node)))
               "\nSource: " (pr-str (:node/source node)))))))

(defn analyse-list [{:keys [children] :as node}]
  (if (= 0 (count children))
    (throw (RuntimeException. "Unquoted empty list not allowed"))
    (let [c1 (analyse-after node (assoc (nth children 0) :invoke-target? true))
          sf (:special-form c1)]
      (if sf
        (sf node)
        (throw (RuntimeException. "cannot invoke"))))))

(defn analyse-vector [{:keys [children] :as node}]
  (if (= 0 (count children))
    (transfer-branch-env node
      {:node/kind :get-field
       :classname "io.lacuna.bifurcan.List"
       :field-name "EMPTY"
       :type (type/obj-classname->type "io.lacuna.bifurcan.List")})
    (let [args (analyse-args node children)
          final (peek args)]
      (transfer-branch-env final
        {:node/kind :jcall
         :classname "io.lacuna.bifurcan.List"
         :method-name "of"
         :method-type (Type/getMethodType "([Ljava/lang/Object;)Lio/lacuna/bifurcan/List;")
         :args [{:node/kind :new-array
                 :classname "java.lang.Object"
                 :ndims 1
                 :dims [(new-const-prim-node final (count children))]
                 :items args}]}))))

(defn analyse-set [{:keys [children] :as node}]
  ;; FIXME too similar to vector
  (let [children (vec children)]
    (if (= 0 (count children))
      (transfer-branch-env node
        {:node/kind :get-field
         :classname "io.lacuna.bifurcan.Set"
         :field-name "EMPTY"
         :type (type/obj-classname->type "io.lacuna.bifurcan.Set")})
      (let [args (analyse-args node children)
            final (peek args)]
        (transfer-branch-env final
          {:node/kind :jcall
           :classname "io.lacuna.bifurcan.Set"
           :method-name "of"
           :method-type (Type/getMethodType "([Ljava/lang/Object;)Lio/lacuna/bifurcan/Set;")
           :args [{:node/kind :new-array
                   :classname "java.lang.Object"
                   :ndims 1
                   :dims [(new-const-prim-node final (count children))]
                   :items args}]})))))

(defn analyse-map [{:keys [children] :as node}]
  (-> (reduce (fn [target [k v]]
                (let [k' (analyse-after target k)
                      v' (analyse-after k' v)]
                  {:node/kind :jcall
                   :classname "io.lacuna.bifurcan.IMap"
                   :interface? true
                   :method-name "put"
                   :method-type (Type/getMethodType "(Ljava/lang/Object;Ljava/lang/Object;)Lio/lacuna/bifurcan/IMap;")
                   :target target
                   :args [k' v']}))
        (transfer-branch-env node
          {:node/kind :get-field
           :classname "io.lacuna.bifurcan.Map"
           :field-name "EMPTY"
           :type (type/obj-classname->type "io.lacuna.bifurcan.Map")})
        children)
    (assoc :node/spec (spec/of-class "io.lacuna.bifurcan.Map"))))

(defn -analyse-node [node]
  (let [f (condp = (:node/kind node)
            :symbol analyse-symbol
            :list analyse-list
            :keyword analyse-keyword
            :vector analyse-vector
            :map analyse-map
            :number analyse-number
            :string analyse-string
            :set analyse-set
            :char analyse-char
            (throw (RuntimeException. (str "AST node analyser not found: " node))))]
    (try (f node)
      (catch Throwable e
        (throw (ex-info "Exception analysing node"
                 {:node (select-keys node [:node/kind :node/source])} e))))))

(defn inject-default-env [node]
  (assoc node :node/env {:class-aliases interop/base-class-aliases
                         :class-resolver interop/find-class
                         :self-classname "_.(Eval)"}))

(defn expand-classname [env s]
  (assert (string? s))
  (or (get (:class-aliases env) s) s))

#_(deftype+ BaseAnaMetaRdrVisitor [parent ^:mut meta]
  PFormVisitor
  (-visitNumber [_ numstr]
    (throw (UnsupportedOperationException.)))
  (-visitKeyword [_ kwstr]
    (set! meta
      {:node/kind :keyword
       :string (subs kwstr 1)}))
  (-visitSymbol [_ symstr]
    (set! meta
      {:node/kind :symbol
       :string symstr}))
  (-visitChar [_ token]
    (throw (UnsupportedOperationException.)))
  (-visitCharLiteral [_ c]
    (throw (UnsupportedOperationException.)))
  (-visitString [_ s]
    (set! meta
      {:node/kind :string
       :value s}))
  (-visitList [self] 
    (throw (UnsupportedOperationException.)))
  (-visitVector [self]
    (throw (UnsupportedOperationException.)))
  (-visitMap [self]
    (BaseAnaRdrVisitor. self {:node/kind :map} (rv-data/clj-map-builder)))
  (-visitSet [self]
    (throw (UnsupportedOperationException.)))
  (-visitDiscard [_] (reader/noop-visitor))
  (-visitEnd [_]
    (when parent
      (rv-data/-addEnd (.-cb ^BaseAnaRdrVisitor parent)
        (assoc node :children (rv-data/-toColl cb)))))
  rv-data/PCollBuilder
  (-toColl [_] (rv-data/-toColl cb)))

(deftype+ BaseAnaMetaBuilder [^:mut meta ^:mut target]
  rv-data/PCollBuilder
  (-addEnd [_ x]
    (if (nil? meta)
      (set! meta x)
      (set! target x)))
  (-toColl [_]
    (letfn [(merge-tag []
              (let [m (:node/meta target)
                    t (:tag m)
                    tk (:node/kind t)]
                (assoc-in target [:node/meta :tag]
                  (cond
                    (nil? t) {:node/kind :vector
                              :children [meta]}
                    (= :vector tk) (conj t meta)
                    (#{:keyword :string :symbol} tk)
                    {:node/kind :vector
                     :children [t meta]}))))]
      (condp = (:node/kind meta)
        :map (assoc target :node/meta meta)
        :keyword (merge-tag)
        :symbol (merge-tag)
        :string (merge-tag)
        (throw (UnsupportedOperationException.))))))

(defn rdr-srcinfo [rdr]
  {:line (reader/-getLine rdr)
   :col (reader/-getCol rdr)})

(deftype BaseAnaRdrVisitor [rdr parent node cb]
  PFormVisitor
  (-visitNumber [_ numstr]
    (let [num (rv-data/parse-number numstr)]
      (if (nil? num)
        (throw (RuntimeException. (str "Invalid number: " numstr)))
        (rv-data/-addEnd cb
          (assoc num
            :node/kind :number
            :node/source (rdr-srcinfo rdr))))))
  (-visitKeyword [_ kwstr]
    (rv-data/-addEnd cb
      {:node/kind :keyword
       :node/source (rdr-srcinfo rdr)
       :string (subs kwstr 1)}))
  (-visitSymbol [_ symstr]
    (rv-data/-addEnd cb
      {:node/kind :symbol
       :node/source (rdr-srcinfo rdr)
       :string symstr}))
  (-visitChar [_ token]
    (rv-data/-addEnd cb 
      {:node/kind :char
       :node/source (rdr-srcinfo rdr)
       :value (rv-data/interpret-char-token token)}))
  (-visitCharLiteral [_ c]
    (rv-data/-addEnd cb
      {:node/kind :char
       :node/source (rdr-srcinfo rdr)
       :value (char c)}))
  (-visitString [_ s]
    (rv-data/-addEnd cb
      {:node/kind :string
       :node/source (rdr-srcinfo rdr)
       :value s}))
  (-visitList [self] 
    (BaseAnaRdrVisitor. rdr self {:node/kind :list} (rv-data/clj-vector-builder)))
  (-visitVector [self]
    (BaseAnaRdrVisitor. rdr self {:node/kind :vector} (rv-data/clj-vector-builder)))
  (-visitMap [self]
    (BaseAnaRdrVisitor. rdr self {:node/kind :map} (rv-data/clj-map-builder)))
  (-visitSet [self]
    (BaseAnaRdrVisitor. rdr self {:node/kind :set} (rv-data/clj-set-builder)))
  (-visitMeta [self]
    (let [v (BaseAnaRdrVisitor. rdr self nil (->BaseAnaMetaBuilder nil nil))]
      [v v]))
  (-visitDiscard [_] (reader/noop-visitor))
  (-visitEnd [_]
    (when parent
      (let [coll (rv-data/-toColl cb)]
        (rv-data/-addEnd (.-cb ^BaseAnaRdrVisitor parent)
          (if node
            (assoc node :children coll
              :node/source (rdr-srcinfo rdr))
            coll)))))
  rv-data/PCollBuilder
  (-toColl [_] (rv-data/-toColl cb)))

(defn base-rdrvisitor [rdr]
  (->BaseAnaRdrVisitor rdr nil nil (rv-data/clj-vector-builder)))
  
(defn str->ast [s]
  (let [rdr (reader/str-reader-default s)
        v (base-rdrvisitor rdr)]
    (reader/read-all-forms v rdr)
    (rv-data/-toColl v)))

(comment
  
  (str->ast "#_(+ 1 #_2)")
  
  )