(ns jl.compiler.nodal.main
  (:require
    [clojure.string :as str]
    [chic.decompiler :as decompiler]
    [jl.kickstart :as kickstart]
    [jl.compiler.core :as compiler]
    [jl.compiler.nodal.jtype :as jtype]
    [jl.compiler.analyser :as ana]
    [jl.compiler.defclass :as dfclass])
  (:import
    (java.nio.file Files Path)))

(defn next-special-comment [asts]
  (let [ast (first asts)]
    (assert (= :special-comment (:node/kind ast)))
    [(:string ast) (vec (rest asts))]))

(defn up-to-special-comment [asts]
  (loop [asts asts 
         asts1 []]
    (if-some [ast (first asts)]
      (if (= :special-comment (:node/kind ast))
        [asts1 (:string ast) (vec (next asts))]
        (recur (next asts) (conj asts1 ast)))
      [asts1 nil nil])))

(defn load-hidden-class-from-classinfo [env classinfo]
  (let[loaded-classes
       (compiler/load-ast-classes-hidden2
         {:lookup (kickstart/get-hidden-class-host-lookup)}
         {:node/env
          (assoc env :new-classes
            {(:classname classinfo) classinfo})})]
    (kickstart/swinvalidate-dyncls)
    (get loaded-classes (:classname classinfo))))

(defn merge-node-ctxs [ctx1 ctx2]
  (update ctx1 :class-aliases merge (:class-aliases ctx2)))

(declare load-node)

(defn asts->context [asts]
  (loop [context {}
         asts asts]
    (if-let [ast1 (first asts)]
      (case (:node/kind ast1)
        :string
        (let [node-id (:value ast1)]
          (recur (update context :imports (fnil conj []) [:node/id node-id])
            (next asts)))
        :list
        (let [[ast1 & body-asts] (:children ast1)]
          (case (:string ast1)
            "Refer-Classes"
            (let [expand identity
                  aliases
                  (into (:class-aliases context {})
                    (mapcat (fn [{:keys [children]}]
                              (let [pkg (:string (first children))
                                    names (map :string (rest children))]
                                (map #(vector % (expand (str pkg "." %))) names))))
                    body-asts)]
              (recur (assoc context :class-aliases aliases)
                (next asts))))))
      context)))

(defn load-node-jtype-v1 [{:keys [node-id]} asts]
  (let [[[ast1] next-header asts] (up-to-special-comment asts)
        clsname (:string ast1)]
    (loop [next-header next-header
           asts asts
           info {:node/kind "jtype-v1"
                 :classname clsname
                 :node/ctx-export
                 {:class-aliases {clsname [:node/id node-id]}}}]
      (case next-header
        "Context"
        (let [[asts1 next-header asts] (up-to-special-comment asts)
              ctx (asts->context asts1)]
          (recur next-header asts (assoc info :node/ctx ctx)))
        "Fields"
        (let [[fasts _ asts] (up-to-special-comment asts)]
          (assoc info :fields-ast fasts))))))

(defn load-node-fndecl-v1 [{:keys [node-id]} asts]
  (let
    [[asts1 next-header asts] (up-to-special-comment asts)
     fnname (:string (first asts1))]
    (assert (string? fnname))
    (loop [next-header next-header
           asts asts
           info {:node/kind "fndecl-v1"
                 :name fnname}]
      (case next-header
       "Params"
       (let [[[params-ast] next-header asts] (up-to-special-comment asts) ]
         (recur next-header asts
           (assoc info :params-ast params-ast)))
        "Body"
        (let [[body-ast next-header asts] (up-to-special-comment asts)]
          (assoc info :body-ast body-ast))
       "Context"
       (let [[asts1 next-header asts] (up-to-special-comment asts)
             ctx (asts->context asts1)]
         (recur next-header asts (assoc info :node/ctx ctx)))
       (throw (ex-info "missing a fndecl attribute" {}))))))

(defn load-node-jclass-v1 [{:keys [node-id]} asts]
  (let
    [[[ast1] next-header asts] (up-to-special-comment asts)
     classname (:string ast1)
     _ (assert (string? classname))
     parse-fn (fn [info ast]
                (assert (= :string (:node/kind ast)))
                (update info :static-methods (fnil conj [])
                  [:node/id (:value ast)]))]
    (loop [next-header next-header
           asts asts
           info {:node/kind "jclass-v1"
                 :node/ctx
                 {:class-aliases {classname [:node/id node-id]}}
                 :node/ctx-export
                 {:class-aliases {classname [:node/id node-id]}}
                 :classname classname}]
      (case next-header
        "Functions"
        (let [[asts1 next-header asts] (up-to-special-comment asts)]
          (reduce parse-fn info asts1))
        "Context"
        (let [[asts1 next-header asts] (up-to-special-comment asts)
              ctx (asts->context asts1)]
         (recur next-header asts (assoc info :node/ctx ctx)))
       ))))

(defn load-node-defclass-v1 [{:keys [node-id]} asts]
  (let
    [[[ast1] next-header asts] (up-to-special-comment asts)
     classname (:string ast1)
     _ (assert (string? classname))]
    (loop [next-header next-header
           asts asts
           info {:node/kind "defclass-v1"
                 :node/ctx
                 {:class-aliases {classname [:node/id node-id]}}
                 :node/ctx-export
                 {:class-aliases {classname [:node/id node-id]}}
                 :classname classname}]
      (case next-header
        "Body"
        (let [[asts1 next-header asts] (up-to-special-comment asts)]
          (assoc info :body-ast asts1))
        "Context"
        (let [[asts1 next-header asts] (up-to-special-comment asts)
              ctx (asts->context asts1)]
         (recur next-header asts (assoc info :node/ctx ctx)))
       ))))

(defn load-node-context-v1 [{:keys [node-id]} asts]
  (let [[asts1 next-header asts] (up-to-special-comment asts)
        ctx (asts->context asts1)]
    {:node/kind "context-v1"
     :node/ctx-export ctx}))

(def *file-nodes (atom {}))

(defn expand-loc [loc]
  (if (re-find #"\.sq$" loc)
    loc
    (str loc ".sq")))

(defn load-node [loc]
  (try
    (let
      [path (expand-loc loc)
       filename (str "src4/" path)
       modify-time (Files/getLastModifiedTime
                     (Path/of filename (make-array String 0))
                     (make-array java.nio.file.LinkOption 0))
       existing-node (get @*file-nodes path)]
      (if (= modify-time (:node/src-modified-time existing-node))
        existing-node
        (let
          [contents (slurp filename)
           asts (ana/str->ast contents)
           [header asts] (next-special-comment asts)
           ctx {:node-id loc}
           nodeinfo
           (try
             (case header
               "jtype-v1" (load-node-jtype-v1 ctx asts)
               "fndecl-v1" (load-node-fndecl-v1 ctx asts)
               "jclass-v1" (load-node-jclass-v1 ctx asts)
               "defclass-v1" (load-node-defclass-v1 ctx asts)
               "context-v1" (load-node-context-v1 ctx asts))
             (catch Throwable e
               (throw (ex-info (str "Failed to load specific node: " filename) {} e))))]
          (swap! *file-nodes assoc path nodeinfo)
          (assoc nodeinfo
            :node/src-modified-time modify-time))))
    (catch Throwable e
      (throw (ex-info (str "Failed to load node from source: " loc) {} e)))))

(defn expand-node-classname [node-id]
  (str "_." (str/replace (second (re-matches #"(.+)\.sq" node-id)) "/" ",") ))

(defn resolve-node-context [ctx]
  (let [;*seen-imports (volatile! #{})
        merge-imports
        (fn merge-imports [ctx]
          (reduce merge-node-ctxs
            ctx
            (map (fn [[_ id]]
                   ; (when (contains? @*seen-imports id)
                   ;   (throw (ex-info "circular context dependency"
                   ;            {:nodes @*seen-imports
                   ;             :node/id id})))
                   ; (vswap! *seen-imports conj id)
                   (merge-imports (:node/ctx-export (load-node id))))
              (:imports ctx))))
        ctx (merge-imports ctx)]
    {:class-aliases
     (into {}
       (map (fn [[alias source]]
              (let [full (if (string? source)
                           source
                           (let [[_ id] source
                                 ; node (load-node id)
                                 ; classname (:classname node)
                                 ; _ (assert (string? classname))
                                 ]
                             (expand-node-classname id)))]
                [alias full])))
       (:class-aliases ctx))}))

(defn get-node-class [loc]
  (let [nodeinfo (get @*file-nodes (expand-loc loc))
        result (:eval-result nodeinfo)]
    (when result
      (.lookupClass ^java.lang.invoke.MethodHandles$Lookup
        (:lookup result)))))

(defn hidden-classname->node-id [^String classname]
  (let [[_ ^String m] (re-matches #_#"_\.(.+?)(?:\.0x.+)?"
                        #"_\.(.+?)"classname)]
    (when m
      (str (.replace m "," "/") ".sq"))))

(declare evald-node)

(def ^:dynamic *stub-classes* {})

(defn env-default [node-ctx clsname]
  (-> (ana/env-default)
    (update 
      :class-aliases merge
      (merge (kickstart/hidden-class-aliases clsname)
        (:class-aliases node-ctx)))
    (assoc :class-resolver
      (fn [clsname]
        (or
          (when-some [node-id (hidden-classname->node-id clsname)]
            (or ;(get-node-class node-id)
              (let [node (get @*file-nodes node-id)]
                (:meta-class (:eval-result node)))
              (get *stub-classes* node-id)))
          (kickstart/class-resolver clsname))))))

(defn eval-node-jclass-v1
  [{:keys [node-id] :as eval-env}
   {:keys [static-methods node/ctx] :as node}]
  (let
    [clsname (expand-node-classname node-id)
     ctx (resolve-node-context ctx)
     env (env-default ctx clsname)
     classinfo
     {:classname clsname
      :flags #{:public}}
     classinfo
     (reduce
       (fn [classinfo mthd]
         (let [[_ id] mthd
               mthd-node (evald-node eval-env id)
               _ (assert (string? (:name mthd-node))
                   {:keys (keys mthd-node)})
               env (env-default (resolve-node-context
                                  (:node/ctx mthd-node)) clsname)]
           (try
             (binding [*stub-classes* {node-id (compiler/create-stub-class classinfo)}]
               (dfclass/classinfo-add-method classinfo
                (dfclass/analyse-method-body {:node/env env}
                  classinfo true (:name mthd-node)
                  (:params-ast mthd-node) (:body-ast mthd-node) nil)))
             (catch Throwable e
               (throw (ex-info (str "Failed to analyse method '"
                                 (:name mthd-node) "'")
                        {} e))))))
       classinfo static-methods)
     {lk :lookup
      class-bytes :class-bytes} (load-hidden-class-from-classinfo env classinfo)]
    (assoc node :eval-result
      {:lookup lk
       :class-bytes class-bytes
       :meta-class (dfclass/classinfo->class env classinfo)})))

(defn eval-node-jtype-v1
  [{:keys [node-id]}
   {:keys [fields-ast node/ctx] :as node}]
  (let
    [clsname (expand-node-classname node-id)
     ctx (resolve-node-context ctx)
     env (env-default ctx clsname)
     classinfo
     {:classname clsname
      :flags #{:public}}
     classinfo
     (jtype/parse-fields env classinfo fields-ast)
     {lk :lookup
      class-bytes :class-bytes} (load-hidden-class-from-classinfo env classinfo)]
    (assoc node :eval-result
      {:lookup lk
       :class-bytes class-bytes
       :meta-class (dfclass/classinfo->class env classinfo)})))

(defn eval-node-defclass-v1
  [{:keys [node-id]}
   {:keys [body-ast node/ctx] :as node}]
  (let
    [clsname (expand-node-classname node-id)
     ctx (resolve-node-context ctx)
     env (env-default ctx clsname)
     classinfo ;; FIXME see load-hidden-as
     (ana/-analyse-node classinfo clsname)
     {lk :lookup
      class-bytes :class-bytes} (load-hidden-class-from-classinfo env classinfo)]
    (assoc node :eval-result
      {:lookup lk
       :class-bytes class-bytes
       :meta-class })))

(defn evald-node [{:keys [eval-id nodes-being-evald] :as env} node-id]
  (let
    [node-id (expand-loc node-id)
     nodes-being-evald (or nodes-being-evald #{})
     _ (when (contains? nodes-being-evald node-id)
         (throw (ex-info "Circular dependency during eval"
                  {:nodes nodes-being-evald
                   :node/id node-id})))
     node (load-node node-id)]
    (if (= (:node/eval-id node) eval-id)
      node
      (let
       [env (assoc env
              :node-id node-id
              :nodes-being-evald (conj nodes-being-evald node-id))
        node
        (try
          (run! #(evald-node env (nth % 1))
            (:imports (:node/ctx node)))
          (case (:node/kind node)
            "jclass-v1" (eval-node-jclass-v1 env node)
            "jtype-v1" (eval-node-jtype-v1 env node)
            "defclass-v1" (eval-node-defclass-v1 env node)
            "fndecl-v1" node
            "context-v1"
            (do
              (run! #(evald-node env (nth % 1))
                (:imports (:node/ctx-export node)))
              node))
          (catch Throwable e
            (throw (ex-info (str "Could not eval node " node-id)
                     {} e))))
        node (assoc node :node/eval-id eval-id)]
       (swap! *file-nodes assoc node-id node)
       node))))

(defn eval-node [id]
  (try (evald-node {:eval-id (random-uuid)} id)
    (catch Throwable e
      (if (instance? StackOverflowError e) 
        (throw e)
        (do (.printStackTrace e) e)))))

(defn decompile-node [id lang]
  (let [ba (:class-bytes (:eval-result (get @*file-nodes (expand-loc id))))]
    (when ba
      (println (decompiler/decompile-bytes ba lang)))))

(comment
  (load-node "chic/ui/types/SizeWH")
  
  (eval-node "chic/browser/UiRootObj")
  (eval-node "chic/window/main/Main")
  (decompile-node "chic/window/main/Main" :bytecode-ast)
  
  (->
    (:class-aliases
     (resolve-node-context
       (:node/ctx (load-node "chic/browser/UiRootObj"))))
    (get "Titlebar"))
  (resolve-node-context
    (:node/ctx-export (load-node "chic/browser/inspector/titlebar/Titlebar")))
  
  (let [ctx (:node/ctx (load-node "chic/browser/UiRootObj"))]
    (map (fn [[_ id]]
           (:node/ctx (load-node id)))
      (:imports ctx)))
  
  (:eval-result (load-node "chic/browser/inspector/titlebar/Titlebar.sq"))
  
  
  (keys @*file-nodes)
  
  )
