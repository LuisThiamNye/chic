(ns jl.compiler.analyser
  (:require
    [jl.compiler.type :as type]
    [chic.util :as util :refer [<- deftype+]]
    [jl.rv-data :as rv-data]
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.compiler.op :as op]
    [clojure.core.match :refer [match]]
    [jl.reader :as reader :refer [PFormVisitor]]
    [chic.debug :as debug]
    [jl.interop :as interop])
  (:import
    (org.objectweb.asm Type)))

(defn in-statement? [env]
  (= :statement (:ctx env)))

(defn statement-env [env]
  (assoc env :ctx :statement))

(defn expression-env [env]
  (assoc env :ctx :expression))

(defn give-env-retctx-of [env2 env1]
  (assoc env2 :ctx (:ctx env1)))

(defn transfer-branch-env [prev node]
  (assert (some? (:node/env prev))
    (assoc (select-keys prev [:node/kind]) :keys (keys prev)))
  (conj {:node/locals (:node/locals prev)
         :node/env (:node/env prev)}
    node))

(defn new-void-node
  ([] {:node/kind :void
       :node/spec {:spec/kind :exact-class
                   :classname "void"}})
  ([prev]
   (transfer-branch-env prev
     (if (in-statement? (:node/env prev))
       (new-void-node)
       {:node/kind :nil
        :node/spec {:spec/kind :nil}}))))

(defn get-meta-tags [meta]
  (let [tag (:tag meta)]
    (if (= :vector (:node/kind tag))
      (:children tag)
      [tag])))

(declare -analyse-node)

(defn analyse-after [prev node]
  (assert (map? node))
  (assert (some? (:node/env prev)))
  (assert (some? (:node/locals prev))
    {:msg "prev has no locals"
     :prev (select-keys prev [:node/kind])})
  (let [locals (:node/locals prev)
        ana (-analyse-node
              (cond-> (assoc node :node/env (:node/env prev))
                locals (assoc :node/locals locals)))
        ana (if (contains? ana :node/source)
              ana
              (assoc ana :node/source (:node/source prev)))]
    (assert (some? (:node/locals ana)))
    (assert (some? (:node/env ana))
      {:msg "returned without env"
       :ret (select-keys ana [:node/kind])
       :prev (select-keys prev [:node/kind])})
    (if (and (in-statement? (:node/env prev))
          (not (spec/void? (:node/spec ana))))
      (do
       (assoc ana
         :node/discard-result true
         :node/spec (spec/of-class "void")))
      ana)))

(defn analyse-expr [prev node]
  (update (analyse-after (update prev :node/env expression-env) node)
    :node/env give-env-retctx-of (:node/env prev)))

(defn analyse-args [prev args]
  (if (= 0 (count args))
    []
    (let [n1 (analyse-expr prev (nth args 0))]
      (if (= 1 (count args))
        [n1]
        (reduce
          (fn [acc node]
            (conj acc (analyse-expr (peek acc) node)))
          [n1] (subvec args 1))))))

(defn analyse-body [prev body]
  (let [n (count body)]
    (if (= 0 n)
      (new-void-node prev)
      (let [statements
            (reduce
              (fn [acc node]
                (let [ana (analyse-after
                            (update (or (peek acc) prev) :node/env statement-env)
                            node)]
                  (assert (some? ana))
                  (conj acc ana)))
              [] (subvec body 0 (dec n)))
            tail (analyse-after (update (or (peek statements) prev) :node/env
                                  give-env-retctx-of (:node/env prev))
                   (nth body (dec n)))
            children (conj statements tail)]
        (transfer-branch-env tail
          {:node/kind :do
           :children children
           :node/spec (:node/spec tail)})))))

(defn new-const-prim-node [prev value]
  (transfer-branch-env prev
    {:node/kind :const-prim
     :node/spec {:spec/kind :exact-class
                 :classname (.getClassName
                              (type/unbox(type/obj-classname->type
                                           (.getName (class value)))))}
     :value value}))

(def prim-widening-conversions-
  ;; can't implicitly to char as it is unsigned (would lose negatives)
  (into {}
    (map (fn [[from tos]]
           (let [basic-from (keyword (case from ("byte" "short" "char") "int" from))]
             [(util/primitive-name->class from)
              (into {}
                (map (fn [to]
                       (let [basic-to (keyword (case to ("byte" "short" "char") "int" to))]
                         [(util/primitive-name->class to)
                          (match [basic-from basic-to]
                            [:int :long] :i2l
                            [:int :float] :i2f
                            [:int :double] :i2d
                            [:long :float] :l2f
                            [:long :double] :l2d
                            [:float :double] :f2d
                            :else :nop)])))
               tos)])))
    [["byte" ["short" "int" "long" "float" "double"]]
     ["short" ["int" "long" "float" "double"]]
     ["char" ["int" "long" "float" "double"]]
     ["int" ["long" "float" "double"]]
     ["long" ["float" "double"]]
     ["float" ["double"]]]))

(defn get-implicit-prim-wideconv [cfrom cto]
  (if (identical? cfrom cto)
    [:insn :nop]
    (when-some [op (get (get prim-widening-conversions- cfrom)
                     cto)]
      [:insn op])))

(comment
  (get-implicit-prim-wideconv Integer/TYPE Long/TYPE)
  (get-implicit-prim-wideconv Integer/TYPE Float/TYPE)
  (get-implicit-prim-wideconv Float/TYPE Float/TYPE))

(defn prim-class->box-conversion
  ([^Class cls]
   (interop/make-box-conversion
     (case (.getName cls)
       "boolean" "Boolean"
       "int" "Integer"
       "short" "Short"
       "char" "Character"
       "long" "Long"
       "byte" "Byte"
       "float" "Float"
       "double" "Double")
     (.getName cls)))
  ([cfrom cto]
   (case (.getName ^Class cfrom)
     "boolean" (when (interop/-satisfies? Boolean cto)
                 (interop/make-box-conversion "Boolean" "boolean"))
     "byte" (when (interop/-satisfies? Byte cto)
              (interop/make-box-conversion "Byte" "byte"))
     "short" (when (interop/-satisfies? Short cto)
               (interop/make-box-conversion "Short" "short"))
     "char" (when (interop/-satisfies? Character cto)
              (interop/make-box-conversion "Character" "char"))
     "int" (when (interop/-satisfies? Integer cto)
             (interop/make-box-conversion "Integer" "int"))
     "long" (when (interop/-satisfies? Long cto)
              (interop/make-box-conversion "Long" "long"))
     "float" (when (interop/-satisfies? Float cto)
               (interop/make-box-conversion "Float" "float"))
     "double" (when (interop/-satisfies? Double cto)
                (interop/make-box-conversion "Double" "double")))))

(comment
  (prim-class->box-conversion Integer/TYPE))

(defn get-coercion [env cfrom cto]
  (if (and (interop/-primitive? cfrom) (interop/-primitive? cto))
    (get-implicit-prim-wideconv cfrom cto)
    (if (interop/-primitive? cto)
      (let [fromname (.getName ^Class cfrom)
            [prim boxname]
            (case fromname
              "java.lang.Boolean" [Boolean/TYPE "Boolean"]
              "java.lang.Byte" [Byte/TYPE "Byte"]
              "java.lang.Short" [Short/TYPE "Short"]
              "java.lang.Character" [Character/TYPE "Character"]
              "java.lang.Integer" [Integer/TYPE "Integer"]
              "java.lang.Long" [Long/TYPE "Long"]
              "java.lang.Float" [Float/TYPE "Float"]
              "java.lang.Double" [Double/TYPE "Double"]
              nil)]
        (when prim
          (let [unboxcnv (interop/make-unbox-conversion boxname (.getName ^Class prim))]
            (when-some [c2 (get-implicit-prim-wideconv prim cto)]
              [:do unboxcnv c2]))))
      (if (interop/-primitive? cfrom)
        (prim-class->box-conversion cfrom cto)
        (if (interop/-satisfies? cfrom cto)
          [:insn :nop]
          nil)))))

(comment
  (get-coercion {} Integer/TYPE Integer)
  (get-coercion {} Integer/TYPE Object)
  (get-coercion {} String Object)
  (get-coercion {} Integer/TYPE Integer/TYPE)
  (get-coercion {} String String)
  (get-coercion {} String Object)
  (get-coercion {} Object String)
  (get-coercion {} Character Integer/TYPE))

(defn coerce-to-class [cls node]
  (let [env (:node/env node)
        input-cls (spec/get-duck-class env (:node/spec node))
        co (get-coercion env input-cls cls)]
    (if co
      (assoc node :node/coercion co)
      (throw (ex-info (str "No coercion from " input-cls " to " cls) {})))))

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

(defn anasf-assign [{:keys [children node/env]:as node}]
  (when-not (= 3 (count children))
    (throw (RuntimeException. "set! must have two args")))
  (let [target (nth children 1)
        valdecl (nth children 2)]
    (or
      (when (= :symbol (:node/kind target))
        (let [sym (:string target)
              statement? (in-statement? env)]
          (or
            (when-some [local (get (:node/locals node) sym)]
              (let [v (analyse-expr node valdecl)
                    s (:node/spec v)]
                {:node/kind :assign-local
                 :node/spec (if statement?
                              (spec/of-class "void")
                              (:node/spec v))
                 :local-name sym
                 :val v
                 :statement? statement?
                 :node/env (:node/env v)
                 :node/locals
                 (cond-> (:node/locals v)
                   (nil? (:spec local))
                   (assoc :node/locals
                     (assoc-in (:node/locals node) [sym :spec] s)))}))
            (when-some [field (get-in env [:fields sym])]
              (assert (:mutable field)
                (str "field " sym " not declared mutable"))
              (let [v (analyse-expr node valdecl)]
                (transfer-branch-env v
                  {:node/kind :self-set-field
                   :field-name sym
                   :type (type/classname->type (spec/get-exact-class (:spec field)))
                   :val v
                   :statement? statement?
                   :node/spec (if statement?
                                (spec/of-class "void")
                                (:node/spec v))}))))))
      (throw (ex-info "Could not resolve set! target"
               {:target target})))))
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

(defn make-local-info [name init-spec]
  {:spec init-spec
   :id (str (gensym name))})

(defn anasf-introduce-local [{:keys [children node/env] :as node}]
  (assert (<= 3 (count children)))
  (let [nam (:string (nth children 1))
        _ (assert (string? nam))
        init (analyse-expr node (nth children 2))
        statement? (in-statement? env)
        init-spec (:node/spec init)]
    (assert (not (spec/void? init-spec)))
    {:node/kind :assign-local
     :node/spec (if statement?
                  (spec/of-class "void")
                  (:node/spec init))
     :local-name nam
     :val init
     :statement? statement?
     :node/env (:node/env init)
     :node/locals (assoc (:node/locals init) nam
                    (make-local-info nam init-spec))}))

(defn anasf-let [{:keys [children] :as node}]
  (if (= 1 (count children))
    (transfer-branch-env node (new-void-node))
    (let [pairs (vec (partitionv 2 (subvec children 1)))
          tail (when (even? (count children))
                 (peek children))
          [pairs tail] (if tail [pairs tail]
                         [(do (assert (= :symbol (:node/kind (first (peek pairs)))))
                            (pop pairs)) (peek (peek pairs))])
          assigns
          (reduce
            (fn [acc [sym val]]
              (assert (= :symbol (:node/kind sym)))
              (let [prev (or (peek acc) node)
                    aval (analyse-expr prev val)
                    nam (:string sym)]
                (conj acc
                  (transfer-branch-env aval
                    (if (spec/void? (:node/spec aval))
                      aval
                      {:node/kind :assign-local
                       :local-name nam
                       :val aval
                       :statement? true
                       :node/spec (spec/of-class "void")
                       :node/locals (assoc (:node/locals aval) nam
                                      (make-local-info nam (:node/spec aval)))})))))
            [] pairs)
          tail (analyse-after (or (peek assigns) node) tail)]
      (transfer-branch-env tail
        {:node/kind :do
         :children (conj assigns tail)
         :node/locals (:node/locals node)
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
      "nil" (new-void-node node)
      "true" (new-const-prim-node node true)
      "false" (new-const-prim-node node false)
      nil)
    (when-some [local (get (:node/locals node) string)]
      (assert (some? (:id local)) [string local])
      (transfer-branch-env node
        {:node/kind :local-use
         :node/spec (:spec local)
         ; :local-name string
         :local-id (:id local)
         }))
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
         :field-name string
         :dynamic (:dynamic field)}))
    (when-some [local (get-in node [:node/env :special-locals string])]
      ((:fn local) node))
    (throw (RuntimeException.
             (str "Could not resolve symbol: " string
               "\nLocals: " (pr-str (:node/locals node))
               "\nFields: " (pr-str (:fields (:node/env node)))
               "\nSpecials: " (pr-str (:special-locals (:node/env node)))
               "\nSource: " (pr-str (:node/source node)))))))

(defn analyse-kwlookup [{:keys [children env] :as node}]
  (assert (<= 2 (count children) 3))
  (let [target (analyse-after node (nth children 1))
        target-class (spec/get-duck-class env (:node/spec target))
        fallback-ast (nth children 2 nil)]
    (if (interop/-satisfies? target-class
          (interop/resolve-obj-class env "io.lacuna.bifurcan.IMap"))
      (let
        [mapget
         {:node/kind :jcall
          :node/spec (spec/of-class (if fallback-ast
                                      "java.lang.Object"
                                      "java.util.Optional"))
          :classname "io.lacuna.bifurcan.IMap"
          :interface? true
          :method-name "get"
          :method-type (Type/getMethodType
                         (if fallback-ast
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                           "(Ljava/lang/Object;)Ljava/util/Optional;"))
          :target target
          :args (cond-> [(analyse-keyword (nth children 0))]
                  fallback-ast
                  (conj (analyse-after node fallback-ast)))}]
        (transfer-branch-env (or fallback-ast node)
          (if fallback-ast
            mapget
            {:node/kind :jcall
             :node/spec (spec/of-class "java.lang.Object")
             :classname "java.util.Optional"
             :method-name "get"
             :method-type (Type/getMethodType
                            "()Ljava/lang/Object;")
             :target mapget
             :args []})))
      (throw (ex-info "Unsupported kw lookup target"
               {:target (select-keys target [:node/kind :node/spec])})))))

(defn analyse-list [{:keys [children] :as node}]
  (if (= 0 (count children))
    (throw (RuntimeException. "Unquoted empty list not allowed"))
    (let [ast0 (nth children 0)]
      (if (= :keyword (:node/kind ast0))
        (analyse-kwlookup node)
        (or
          (when (= :symbol (:node/kind ast0))
            (when-some [[_ nspart namepart] (re-matches #"(.+)/(.+)" (:string ast0))]
              ((requiring-resolve 'jl.compiler.sforms/anasf-jscall*)
               nspart namepart node (subvec (:children node) 1))))
          (let [c1 (analyse-after node (assoc ast0 :invoke-target? true))
               sf (:special-form c1)]
           (if sf
             (sf node)
             (throw (RuntimeException. "cannot invoke")))))))))

(defn node-as-class [env cto node]
  (let [cls (spec/get-duck-class env (:node/spec node))]
    (if (interop/same-class? cto cls)
      node
      (let [coercion (get-coercion env cls cto)]
        (if coercion
          (assoc node :node/coercion coercion)
          (throw (ex-info "no coercion for node"
                   {:node (select-keys node [:node/kind :node/source])})))))))

(defn analyse-vector [{:keys [children node/env] :as node}]
  (let [clsname "io.lacuna.bifurcan.List"
        spec (spec/of-class clsname)]
    (if (= 0 (count children))
     (transfer-branch-env node
       {:node/kind :get-field
        :classname clsname
        :field-name "EMPTY"
        :field-type (type/obj-classname->type clsname)
        :node/spec spec})
     (let [args (analyse-args node children)
           args (mapv (partial node-as-class env Object) args)
           final (peek args)]
       (transfer-branch-env final
         {:node/kind :jcall
          :classname clsname
          :method-name "of"
          :method-type (Type/getMethodType "([Ljava/lang/Object;)Lio/lacuna/bifurcan/List;")
          :node/spec spec
          :args [{:node/kind :new-array
                  :classname "java.lang.Object"
                  :ndims 1
                  :dims [(new-const-prim-node final (count children))]
                  :items args}]})))))

(defn analyse-set [{:keys [children node/env] :as node}]
  ;; FIXME too similar to vector
  (let [children (vec children)
        clsname "io.lacuna.bifurcan.Set"
        spec (spec/of-class clsname)]
    (if (= 0 (count children))
      (transfer-branch-env node
        {:node/kind :get-field
         :classname clsname
         :field-name "EMPTY"
         :field-type (type/obj-classname->type clsname)
         :node/spec spec})
      (let [args (analyse-args node children)
            args (mapv (partial node-as-class env Object) args)
            final (peek args)]
        (transfer-branch-env final
          {:node/kind :jcall
           :classname clsname
           :method-name "of"
           :method-type (Type/getMethodType "([Ljava/lang/Object;)Lio/lacuna/bifurcan/Set;")
           :node/spec spec
           :args [{:node/kind :new-array
                   :classname "java.lang.Object"
                   :ndims 1
                   :dims [(new-const-prim-node final (count children))]
                   :items args}]})))))

(defn analyse-map [{:keys [children node/env] :as node}]
  (let [clsname "io.lacuna.bifurcan.Map"]
    (-> (reduce
          (fn [target [k v]]
            (let [k' (node-as-class env Object (analyse-expr target k))
                  v' (node-as-class env Object (analyse-expr k' v))]
              (transfer-branch-env v'
                {:node/kind :jcall
                 :classname "io.lacuna.bifurcan.IMap"
                 :interface? true
                 :method-name "put"
                 :method-type (Type/getMethodType
                                "(Ljava/lang/Object;Ljava/lang/Object;)Lio/lacuna/bifurcan/IMap;")
                 :target target
                 :args [k' v']})))
          (transfer-branch-env node
            {:node/kind :get-field
             :classname clsname
             :field-name "EMPTY"
             :field-type (type/obj-classname->type clsname)})
          children)
      (as-> m
        (transfer-branch-env m
          {:node/kind :cast
           :type (type/classname->type clsname)
           :body m
           :node/spec (spec/of-class clsname)})))))

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
            (throw (RuntimeException.
                     (str "AST node analyser not found: "
                       (select-keys node [:node/kind :node/source])))))]
    (try (f node)
      (catch Throwable e
        (throw (ex-info "Exception analysing node"
                 {:node (select-keys node [:node/kind :node/source])} e))))))

(defn inject-default-env [node]
  (assoc node :node/env
    {:class-aliases interop/base-class-aliases
     :class-resolver interop/find-class
     :self-classname "_.(Eval)"
     :ctx :expression
     :self-classinfo {:classname "_.(Eval)"
                      :class (interop/new-unreal-class
                               "_.(Eval)" Object nil)}}
    :node/locals {}))

(defn expand-classname [env s]
  (assert (string? s))
  (let [[_ a b c] (re-matches #"(\[+L)?(.+?)(;)?" s)
        b (or (get (:class-aliases env) b) b)
        ret (str a b c)]
    ret))

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
                    (= :vector tk) (update t :children conj meta)
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