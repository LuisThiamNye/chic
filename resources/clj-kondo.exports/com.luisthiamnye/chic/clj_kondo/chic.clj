(ns clj-kondo.chic
  (:require 
    [clj-kondo.hooks-api :as api]))

(defn <- [{:keys [node]}]
  {:node
   (assoc node :children 
     (cons (api/token-node `->>)
       (reverse (next (:children node)))))})

(defn inherit-vars [{:keys [node]}]
  {:node
   (let [syms (mapv (comp api/token-node symbol name :value) 
                (next (:children node)))]
     (api/list-node (list* (api/token-node `declare) syms)))})

(defn loopr [{:keys [node]}]
  {:node
   (let [children (vec (next (:children node)))
         collvec (nth children 0 nil)
         body-or-acc (nth children 1 nil)
         acc? (api/vector-node? body-or-acc)
         accvec (if acc?
                  body-or-acc
                  (api/vector-node []))
         body-idx (if acc? 2 1)
         body (nth children body-idx nil)
         completer (nth children (inc body-idx) nil)]
     (api/list-node
       (list (api/token-node `let)
         collvec
         (api/list-node
           (list (api/token-node `loop)
             accvec
             completer
             body)))))})

(comment
  (-> (api/parse-string (pr-str '([a] b c)))
    :children first api/vector-node?)
  
  (-> (api/parse-string "(loopr [fld fields] [acc nil] body comp)")
    (->> (hash-map :node))
    (loopr)
    :node
    api/sexpr)
  
  (require '[clj-kondo.core :as kondo])
  
  (-> (binding [api/*reload* true]
        (with-in-str (pr-str '(do (in-ns 'chic.util)
                                (declare inherit-vars)
                                (inherit-vars n/a n/b)))
                 (kondo/run! {:lint ["-"]
                              :cache false})))
    ;:config :analyze-call
    :findings)

  )