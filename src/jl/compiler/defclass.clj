(ns jl.compiler.defclass
  (:require
    [chic.util :refer [loop-zip]]
    [jl.interop :as interop]
    [jl.compiler.spec :as spec]
    [jl.compiler.core :as comp]
    [jl.compiler.op :as op]
    [jl.compiler.analyser :as ana :refer [-analyse-node]]
    [jl.compiler.type :as type])
  (:import
    (org.objectweb.asm Type)))

(defn anasf-init-super [{:keys [children node/env] :as node}]
  (let [args (ana/analyse-args node (subvec children 1))
        prev-node (or (peek args) node)
        classname (:super-classname (:node/env node))]
    (ana/transfer-branch-env prev-node
      {:node/kind :jcall-super
       :classname classname
       :method-type
       (interop/match-ctor-type env classname
         (mapv (comp spec/get-exact-class :node/spec) args))
       :args args
       :node/spec {:spec/kind :exact-class
                   :classname "void"}})))

(defn class-tag [{:keys [node/env] :as node}]
  (assert (some? env))
  (let [tag (:tag (:node/meta node))]
    (when (= :vector (:node/kind tag))
      (let [s (some #(cond
                       (= :symbol (:node/kind %)) (:string %)
                       (= :string (:node/kind %)) (:value %))
                (:children tag))
            ret (if (= "Self" s) (:self-classname env)
                  (ana/expand-classname env s))]
        
        ;; note: could be primitive
        ; (assert (some #{\.} ret)
        ;   (pr-str ret " env: " (keys env)))
        ret))))

(defn classinfo->class [env info]
  (interop/new-unreal-class
    (:classname info)
    (interop/resolve-class env (:super info "java.lang.Object"))
    (mapv (partial interop/resolve-class env)
      (:interfaces info))
    {:hidden? (if (contains? info :hidden?)
                (:hidden? info)
                (.startsWith (:classname info) "_."))
     :flags (op/kws->acc-mask (:flags info))
     :methods
     (mapv (comp
             (fn [m]
               (-> m
                 (update :flags op/kws->acc-mask)
                 (dissoc :param-classnames :ret-classname)
                 (assoc :param-types (mapv (partial interop/resolve-class env)
                                       (:param-classnames m)))
                 (cond-> (:ret-classname m)
                   (assoc :ret-type (interop/resolve-class env (:ret-classname m))))))
             #(select-keys % [:name :param-classnames :ret-classname :flags]))
       (into (:class-methods info) (:instance-methods info)))
     :fields (mapv (comp
                     (fn [m]
                       (-> m
                         (update :flags op/kws->acc-mask)
                         (dissoc :classname)
                         (assoc :type (interop/resolve-class env (:classname m)))))
                     #(select-keys % [:name :classname :flags]))
               (into (:class-fields info) (:fields info)))
     :constructors
     (mapv (comp
             (fn [m]
               (-> m
                 (update :flags op/kws->acc-mask)
                 (dissoc :param-classnames :ret-classname)
                 (assoc :param-types (mapv (partial interop/resolve-class env)
                                       (:param-classnames m)))
                 (assoc :ret-type Void/TYPE)))
             #(select-keys % [:name :param-classnames :ret-classname :flags]))
       (:constructors info))}))

(defn adapt-method-env
  [{:keys [classname super] :as clsinfo} ret-classname {:keys [method] :as env}]
  (let [super (or super "java.lang.Object")]
    (-> (if (= "void" ret-classname)
          (ana/statement-env env)
          (ana/expression-env env))
      (assoc :fields (into {}
                       (map (fn [{:keys [name classname flags dynamic]}]
                              [name {:spec (spec/of-class classname)
                                     :dynamic dynamic
                                     :mutable (or (:initialiser? method)
                                                (not (contains? flags :final)))}]))
                       (when (= :instance (:side method))
                         (:fields clsinfo))))
      (assoc :self-classname classname)
      (assoc :super-classname super)
      (assoc-in [:new-classes classname]
        (assoc clsinfo :class (classinfo->class env clsinfo)))
      (assoc-in [:class-aliases "Self"] classname)
      (assoc-in [:class-aliases "Super"] super))))

(defn analyse-method-from-sig [env clsinfo static? mn paramdecl retc]
  (let [params (:children paramdecl)
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
        _ (when (and retc tagged-ret)
            (println "WARNING - ineffective return type tag " tagged-ret
              ", predetermined " retc))
        preknown-ret-classname (or retc tagged-ret)
        varargs? (= :exact-array
                   (:spec/kind (:spec (peek (last param-pairs)))))]
    {:name mn
     :flags (cond-> #{:public} 
              varargs? (conj :varargs)
              static? (conj :static))
     :ret-classname preknown-ret-classname
     :param-names (mapv :string params)
     :node/locals (into {} param-pairs)
     :param-classnames
     (mapv (fn [[_ {:keys [spec]}]]
             (spec/get-exact-class spec))
       (if static? param-pairs (drop 1 param-pairs)))}))

(defn analyse-method-body
  [prev-node clsinfo static? mn paramdecl bodydecl retc]
  (let [env (:node/env prev-node)
        method (analyse-method-from-sig env clsinfo static? mn paramdecl retc)
        clsinfo (update clsinfo (if static? :class-methods :instance-methods)
                  (fnil conj []) method)
        prev-method (:method env)
        preknown-ret-classname (:ret-classname method)
        body (ana/analyse-body
               {:node/env (assoc (adapt-method-env clsinfo preknown-ret-classname
                                   (assoc env :method
                                     {:side (if static? :class :instance)
                                      :initialiser? (#{"<clinit>" "<init>"} mn)}))
                            :external-locals (:node/locals prev-node))
                :node/locals (:node/locals method)}
               bodydecl)
        body (assoc body :node/locals (:node/locals prev-node))
        body (update body :node/env assoc :method prev-method)
        ret-classname (or preknown-ret-classname
                        (spec/get-exact-class (:node/spec body))
                        "void")]
    (assoc method :body body
      :ret-classname ret-classname)))

(defn anasf-defclass [{:keys [children node/env] :as node}]
  (assert (<= 2 (count children)))
  (let
    [clsexpr (nth children 1)
     _ (assert (= :symbol (:node/kind clsexpr)))
     clsname (ana/expand-classname env (:string clsexpr))
     make-class (fn [info] (classinfo->class env info))
     iface-parser
     (fn [opts {:keys [node/kind] :as x}]
       (when (= :symbol kind)
         (let [interfaces ((fnil conj []) (:interfaces opts)
                           (ana/expand-classname env (:string x)))]
           (-> opts
             (assoc :interfaces interfaces)
             (as-> info (assoc info :class (make-class info)))))))
     super-parser
     (fn [opts {:keys [node/kind string]}]
       (if (= :symbol kind)
         (let[super (ana/expand-classname env string)]
           (-> opts
             (dissoc :parser)
             (assoc :super super)
             (as-> info (assoc info :class (make-class info)))))
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
     parse-fieldvec
     (fn [opts x]
       (let [flds (reduce (fn [fields f]
                            (if (= :symbol (:node/kind f))
                              (let [cn (or (class-tag (assoc f :node/env env))
                                         "java.lang.Object")
                                    tags (ana/get-meta-tags (:node/meta f))
                                    tags (into #{} (comp (filter (comp #{:keyword} :node/kind))
                                                     (map :string))
                                           tags)
                                    mut-flag (if (tags "mut")
                                               nil
                                               :final)
                                    pub-flag (if (tags "pub")
                                               :public
                                               :private)]
                                (conj fields
                                 {:name (:string f)
                                  :flags (cond-> #{}
                                           mut-flag (conj mut-flag)
                                           pub-flag (conj pub-flag))
                                  :dynamic (interop/dynamic-class?
                                              env (interop/resolve-class env cn))
                                  :classname cn}))
                              (throw (UnsupportedOperationException.))))
                    [] (:children x))]
         (-> opts (assoc :fields flds)
           (update :constructors (fnil conj [])
             {:param-classnames (mapv :classname flds)
              :auto true}))))
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
                        {:flags #{:static}}
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
             init (ana/analyse-expr (update (:prev-node opts) :node/env
                                       (partial adapt-method-env opts nil)) initdecl)
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
                         :field-type (type/classname->type fcls)
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
             env' (:node/env (:prev-node opts))
             paramdecl (assoc (nth children 2) :node/env env')
             params (:children paramdecl)
             ret-classname (or (class-tag paramdecl) "void")]
         (update opts :instance-methods conj
           {:name mn
            :flags #{:abstract :public} ;; either pub or priv must be set
            :ret-classname ret-classname
            :param-names (into [""] (map :string) params)
            :param-classnames
            (mapv (fn [p]
                    (class-tag (assoc p :node/env env')))
              params)})))
     parse-declare-cmethod
     (fn [info {:keys [children]}]
       (assert (<= 2 (count children)))
       (let [mexp (nth children 1)
             _ (assert (= :symbol (:node/kind mexp)))
             mn (:string mexp)
             env' (:node/env (:prev-node info))
             paramdecl (assoc (nth children 2) :node/env env')
             method (analyse-method-from-sig env' info true mn paramdecl nil)]
         (-> info
           (update :class-methods conj method))))
     parse-named-cmethod
     (fn [opts mn paramdecl bodydecl retc]
       (let [method (analyse-method-body (:prev-node opts) opts true mn paramdecl bodydecl retc)
             all-methods (:class-methods opts [])
             [conflict-idx conflicting-method]
             (loop-zip [m all-methods
                        i :idx]
               []
               (if (and (= mn (:name m))
                     (= (:ret-classname method) (:ret-classname m))
                     (= (:param-classnames method) (:param-classnames m)))
                 (do [i m])
                 (recur))
               nil)
             all-methods
             (if conflicting-method
               (if (:body conflicting-method)
                 (throw (ex-info "Conflicting method" {}))
                 (into (subvec all-methods 0 conflict-idx)
                   (subvec all-methods (inc conflict-idx))))
               all-methods)]
         (-> opts
           (assoc :prev-node (:body method))
           (assoc :class-methods (conj all-methods method)))))
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
       (let [m (analyse-method-body (:prev-node opts)
                 opts false mn paramdecl bodydecl retc)]
         (-> opts
           (assoc :prev-node (:body m))
           (update :instance-methods conj m))))
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
     parse-init
     (fn [clsinfo {:keys [children]}]
       (assert (<= 2 (count children)))
       (let [paramsdecl (nth children 1)
             bodydecl (subvec children 2)
             _ (assert (<= 1 (count (:children paramsdecl))) "Must specify 'self' parameter")
             m (analyse-method-body (:prev-node clsinfo)
                 clsinfo false "<init>" paramsdecl bodydecl "void")]
         (-> clsinfo
           (assoc :prev-node (:body m))
           (update :constructors conj m))))
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
                    {:node/env (adapt-method-env clsinfo "void" (:node/env (:prev-node clsinfo)))
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
         (-> clsinfo
           (assoc :prev-node body)
           (update-in [:constructors auto-idx] assoc :body body))))
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
                      "declare-fn" (parse-declare-cmethod opts dnode)
                      "init-auto" (parse-init-auto opts dnode)
                      "init" (parse-init opts dnode)
                      "uninstall" (parse-uninstall-method opts dnode))))
     info {:classname clsname
           :prev-node (update node :node/env assoc :self-classname clsname)}
     info (assoc info :class (make-class info))
     info
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
        info (subvec children 2))
     ; {:keys [prev-node]} info
     ctors (:constructors info)
     auto-ctor-idx (first (keep-indexed #(when (:auto %2) %)
                            ctors))
     info (if (and auto-ctor-idx (< 1 (count ctors)))
            (assoc info :constructors
              (into (subvec ctors 0 auto-ctor-idx)
                (subvec ctors (inc auto-ctor-idx))))
            info)
     info (assoc info :class (make-class info))
     prev-node (:prev-node info)
     info (dissoc info :prev-node)
     verify-methods (fn [methods]
                      (doseq [m methods]
                        (when (nil? (:body m))
                          (throw (ex-info (str (:name m) " has no body")
                                   (select-keys m [:ret-classname :param-classnames]))))))]
    (verify-methods (:class-methods info))
    (verify-methods (:instance-methods info))
    (assoc (ana/new-void-node)
      :node/locals (:node/locals node)
      :node/env (-> (:node/env prev-node)
                  (assoc :self-classname (:self-classname env))
                  (assoc-in [:new-classes clsname] info)))))

(defn anasf-reify [{:keys [children node/env] :as node}]
  ;; TODO - match methods with interfaces; allow extra methods if marked private
  ;; also allow overrides
  ;; enforce implementing all interface methods?
  ;; TODO locals capturing
  #_"(reify ?super <iface|method>*)"
  (assert (<= 2 (count children)))
  (let
    [id (:reify-id env 1)
     env (assoc env :reify-id (inc id))
     node (assoc node :node/env env)
     reify-classname (str (:self-classname env) "$reify" id)
     parse-method
     (fn [{:keys [prev-node] :as info} {:keys [children] :as node}]
       (assert (<= 2 (count children))
         (str "Method decl must specify name+params\n" node))
       (let [method-name (:string (first children))
             _ (assert (string? method-name))
             method (analyse-method-body prev-node info false method-name
                      (nth children 1) (subvec children 2) nil)
             captured-locals (:captured-locals (:node/env (:body method)))
             mt (try
                  (interop/match-method-type (:node/env prev-node) (:class info)
                   false method-name (:param-classnames method) (:ret-classname method))
                  (catch Exception e
                    (throw (ex-info "reify method not defined in interface/super"
                             {:method-name method-name} e))))]
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
                      (assoc info :super classname)))
             super (interop/resolve-class env (:super info))
             outer-classname (:self-classname env)
             hidden? (interop/-hiddenClass? 
                       (interop/resolve-class env outer-classname))
             info (assoc info
                    :class (classinfo->class (:node/env (:prev-node info))
                             (assoc info :hidden? hidden?)))]
         (assoc info :prev-node
           (assoc-in (:prev-node info)
             [:node/env :new-classes reify-classname] info))))
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
