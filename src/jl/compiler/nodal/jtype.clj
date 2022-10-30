(ns jl.compiler.nodal.jtype
  (:require
    [jl.compiler.defclass :as defclass]
    [jl.interop :as interop]
    [jl.compiler.analyser :as ana]))

(defn parse-fields [env classinfo asts]
  (let [flds (reduce
               (fn [fields f]
                 (if (= :symbol (:node/kind f))
                   (let [cn (or (defclass/class-tag (assoc f :node/env env))
                              "java.lang.Object")
                         tags (ana/get-meta-tags (:node/meta f))
                         tags (into #{} (comp (filter (comp #{:keyword} :node/kind))
                                          (map :string))
                                tags)
                         mut-flag (if (tags "mut")
                                    nil
                                    :final)
                         ; pub-flag (if (tags "pub")
                         ;            :public
                         ;            :private)
                         pub-flag :public
                         ]
                     (conj fields
                       {:name (:string f)
                        :flags (cond-> #{}
                                 mut-flag (conj mut-flag)
                                 pub-flag (conj pub-flag))
                        ; :dynamic (interop/dynamic-class?
                        ;            env (interop/resolve-class env cn))
                        :classname cn}))
                   (throw (UnsupportedOperationException.))))
               [] asts)]
    (-> classinfo
      (assoc :fields flds)
      (update :constructors (fnil conj [])
        {:param-classnames (mapv :classname flds)
         :auto true}))))