(ns jl.compiler.defclass
  (:require
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [jl.compiler.analyser :as ana :refer [-analyse-node]]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)))

(defn anasf-init-super [{:keys [children] :as node}]
  (let [args (ana/analyse-args node (subvec children 1))
        prev-node (or (peek args) node)
        classname (:super-classname (:node/env node))]
    (ana/transfer-branch-env prev-node
      {:node/kind :jcall-super
       :classname classname
       :method-name "<init>"
       :method-type
       (interop/match-ctor-type classname
         (mapv (comp spec/get-exact-class :node/spec) args))
       :args args
       :node/spec {:spec/kind :exact-class
                   :classname "void"}})))

(defn class-tag [{:keys [node/env] :as node}]
  (let [tag (:tag (:node/meta node))]
    (when (= :vector (:node/kind tag))
      (let [s (some #(cond
                       (= :symbol (:node/kind %)) (:string %)
                       (= :string (:node/kind %)) (:value %))
                (:children tag))]
        (if (= "Self" s) (:self-classname env)
          (ana/expand-classname env s))))))

(defn adapt-method-env [{:keys [classname super] :as clsinfo} env]
  (-> env
    (assoc :self-classname classname)
    (assoc :super-classname (or super "java.lang.Object"))
    (assoc-in [:new-classes classname] clsinfo)
    (assoc-in [:class-aliases "Self"] classname)))

(defn analyse-method-body
  [prev-node clsinfo static? mn paramdecl bodydecl retc]
  (let [env (:node/env prev-node)
        params (:children paramdecl)
        tagged-ret (class-tag (assoc paramdecl :node/env env))
        param-pairs
        (into []
          (map-indexed
            (fn [i {:keys [string] :as arg-node}]
              (assert (= :symbol (:node/kind arg-node)))
              [string {:arg-idx i
                       :spec (if (and (not static?) (= 0 i))
                               ;; first arg is self
                               (spec/of-class (:classname clsinfo))
                               (if-some [cn (class-tag (assoc arg-node :node/env env))]
                                 (spec/of-class cn)
                                 (spec/of-class "java.lang.Object")))}]))
          params)
        body (ana/analyse-body
               {:node/env (assoc (adapt-method-env clsinfo (:node/env prev-node))
                            :external-locals (:node/locals prev-node))
                :node/locals (into {} param-pairs)}
               bodydecl)
        _ (when (and retc tagged-ret)
            (println "WARNING - ineffective return type tag " tagged-ret
              ", predetermined " retc))
        ret-classname (or retc tagged-ret
                        (spec/get-exact-class (:node/spec body))
                        "void")
        varargs? (= :exact-array
                   (:spec/kind (:spec (peek (last param-pairs)))))]
    {:name mn
     :body body
     :flags (cond-> #{:public} varargs? (conj :varargs))
     :ret-classname ret-classname
     :param-names (mapv :string params)
     :param-classnames
     (mapv (fn [p]
             (let [c (class-tag p)]
               ;(assert (some? c) (str "no tag for " p))
               (or c "java.lang.Object")))
       (if static? params (drop 1 params)))}))

(defn anasf-defclass [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let
    [clsexpr (nth children 1)
     _ (assert (= :symbol (:node/kind clsexpr)))
     clsname (:string clsexpr)
     [clsname env] (if (:def-classes-to-underscore env)
                     (let [n2 (str "_." (.replace clsname \. \,))]
                       [n2 (update env :class-aliases assoc
                             clsname n2)])
                     [clsname env])
     iface-parser (fn [opts {:keys [node/kind] :as x}]
                    (when (= :symbol kind)
                      (update opts :interfaces (fnil conj [])
                        (ana/expand-classname env (:string x)))))
     super-parser (fn [opts {:keys [node/kind string]}]
                    (if (= :symbol kind)
                      (assoc (dissoc opts :parser)
                        :super (ana/expand-classname env string))
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
                (ana/get-meta-tags (:node/meta n)))
     class-tag (fn [n]
                 (class-tag n))
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
                 (adapt-method-env clsinfo env1))
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
     ;; must call direct superclass (or via other self-ctor) before accessing instance members
     ;; but self class fields may be set first
     parse-init-auto
     (fn [clsinfo {:keys [children]}]
       (assert (<= 2 (count children)))
       (let [params-ast (nth children 1)
             _ (assert (= :vector (:node/kind params-ast)))
             params (:children params-ast)
             _ (assert (= 1 (count params)))
             self-sym-ast (nth params 0)
             _ (assert (= :symbol (:node/kind self-sym-ast)))
             self-sym (:string self-sym-ast)
             body (ana/analyse-body
                    {:node/env (adapt-env clsinfo (:node/env (:prev-node clsinfo)))
                     :node/locals
                     (into {self-sym {:arg-idx 0
                                      :spec {:spec/kind :exact-class
                                             :classname clsname}}}
                       (map-indexed (fn [i {:keys [name classname]}]
                                      [name {:arg-idx (inc i)
                                             :spec {:spec/kind :exact-class
                                                    :classname classname}}]))
                       (:fields clsinfo))}
                    (subvec children 2))
             auto-idx (first (keep-indexed #(when (:auto %2) %)
                               (:constructors clsinfo)))]
         (update-in clsinfo [:constructors auto-idx]
           assoc :body body)))
     parse-decl (fn [opts {:keys [children] :as dnode}]
                  (assert (<= 2 (count children)))
                  (comp/create-stub-class opts)
                  (let [decl (nth children 0)
                        _ (assert (= :symbol (:node/kind decl)))
                        dname (:string decl)]
                    (condp = dname
                      "defi" (parse-imethod opts dnode)
                      "defn" (parse-cmethod opts dnode)
                      "defabstract" (parse-amethod opts dnode)
                      "def" (parse-cfield opts dnode)
                      "init-auto" (parse-init-auto opts dnode)
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

(defn anasf-reify [{:keys [children node/env] :as node}]
  ;; TODO - match methods with interfaces; allow extra methods if marked private
  ;; also allow overrides
  ;; enforce implementing all interface methods?
  ;; TODO locals capturing
  #_"(reify ?super <iface|method>*)"
  (assert (<= 2 (count children)))
  (let
    [parse-method
     (fn [{:keys [prev-node] :as info} {:keys [children] :as node}]
       (assert (<= 2 (count children))
         (str "Method decl must specify name+params\n" node))
       (let [method-name (:string (first children))
             _ (assert (string? method-name))
             method (analyse-method-body prev-node info false method-name
                      (nth children 1) (subvec children 2) nil)
             captured-locals (:captured-locals (:node/env (:body method)))]
         (-> (update info :captured-locals (fnil into #{}) captured-locals)
           (update :instance-methods conj method))))
     parse-interface
     (fn [info {:keys [string]}]
       (let [classname (ana/expand-classname env string)
             cls ((:class-resolver env) classname)
             info (if (interop/-interface? cls)
                    (cond-> (update info :interfaces (fnil conj []) classname)
                      (not (:super info))
                      (assoc :super "java.lang.Object"))
                    (if (:super info)
                      (throw (ex-info "Can't have multiple supers" {}))
                      (assoc info :super classname)))]
         (assoc info :class (interop/new-unreal-class
                              (interop/resolve-class env (:super info))
                              (mapv (partial interop/resolve-class env)
                                (:interfaces info))))))
     reify-classname (str (:self-classname env) (gensym "$reify"))
     info
     (reduce (fn [info {:keys [node/kind] :as node}]
               (condp = kind
                 :list (parse-method info node)
                 :symbol (parse-interface info node)))
       {:prev-node node
        :classname reify-classname} (subvec children 1))
     final-node (:prev-node info)
     locals (:node/locals node)
     captured-local-names (:captured-locals info)
     captured-locals (mapv #(get locals %) captured-local-names)
     captured-local-classnames (mapv (comp spec/get-exact-class :spec) captured-locals)
     info (assoc info
            :fields (mapv (fn [name  classname]
                            {:name (str "%" name)
                             :flags #{:private :final}
                             :classname classname})
                      captured-local-names
                      captured-local-classnames)
            :constructors
            [{:param-classnames captured-local-classnames
              :auto true}])
     info (dissoc info :prev-node :captured-locals)]
    {:node/kind :new-obj
     :node/spec (spec/of-class reify-classname)
     :classname reify-classname
     :method-type (Type/getMethodType Type/VOID_TYPE
                    (into-array Type (mapv type/classname->type captured-local-classnames)))
     :args (mapv (fn [name {:keys [spec]}]
                   {:node/kind :local-use
                    :node/spec spec
                    :local-name name})
             captured-local-names captured-locals)
     :node/locals (:node/locals node)
     :node/env (assoc-in (:node/env final-node)
                 [:new-classes reify-classname] info)}))

(swap! ana/*sf-analysers assoc "reify" #'anasf-reify)
(swap! ana/*sf-analysers assoc "init-super" #'anasf-init-super)
