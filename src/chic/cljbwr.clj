(ns chic.cljbwr
  (:require
   [chic.clj.source :as clj.source]
   [io.github.humbleui.paint :as huipaint]
   [chic.cljbwr.var-editor :as var-editor]
   [chic.ui.error :as ui.error]
   [chic.ui :as cui]
   [chic.ui.event :as uievt]
   [chic.text-editor :as text-editor]
   [chic.text-editor.core :as text-editor.core]
   [chic.ui.layout :as cuilay]
   [clojure.repl :as repl]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.skija Paint]))

#_(def editor (text-editor.core/make
               {:pos 0
                :face-default face-default}))

(def *selected-ns (atom nil))
(def *selected-member (atom nil))

#_(defn ns-tree-existing-only
    ([]
     (ns-tree [] (sort-by str (all-ns))))
    ([children rest-ns]
     (if-let [ns (first rest-ns)]
       (let [[children' rest-ns'] (ns-tree [] ns (next rest-ns))]
         (recur (conj children {:ns ns :children children' :string (str ns)})
                (next rest-ns')))
       children))
    ([children parent-ns rest-ns]
     (if-let [ns (first rest-ns)]
       (let [parent-prefix (str parent-ns \.)]
         (if (str/starts-with? (str ns) parent-prefix)
           (let [[children' rest-ns'] (ns-tree [] ns (next rest-ns))]
             (recur (conj children {:ns ns :children children' :string (subs (str ns) (count parent-prefix))})
                    parent-ns rest-ns'))
           [children rest-ns]))
       [children nil])))

(defn ns-tree-map []
  (reduce (fn [tree ns]
            (let [segs (str/split (str ns) #"\.")]
              (if (= "chic" (first segs))
                (assoc-in tree segs {:ns ns})
                tree)))
          {}
          (all-ns)))

(comment
  (ns-tree)

  #!
  )

(defn ns-list-item [{:keys [selected-ns font-ui fill-text] :as ctx} node]
  (let [string (str (:last-seg node))
        ns (:ns node)
        children (map (fn [[k v]] (assoc v :last-seg k))
                      (sort-by key (dissoc node :ns :last-seg)))
        el (ui/fill
            (huipaint/fill (if (and selected-ns (= ns selected-ns))
                             0xFFC0FF90
                             0x00000000))
            (cuilay/padding
             2 5
             (ui/label string font-ui (cond-> ^Paint fill-text
                                        (nil? ns)
                                        (-> .makeClone (doto (.setAlpha (unchecked-int 0x90))))))))]
    (cuilay/column
     (if ns
       (cui/clickable
        (uievt/on-primary-down (fn [_](reset! *selected-ns ns)))
        el)
       el)
     (when children
       (cuilay/row
        (ui/gap 10 0)
        [:stretch 1
         (cuilay/column
          (map (fn [child] (ns-list-item ctx child)) children))])))))

(defn ns-list-view []
  (cuilay/vscrollbar
   (cuilay/vscroll
    (ui/dynamic ctx [font-ui (:font-ui ctx)
                     fill-text (:fill-text ctx)
                     selected-ns @*selected-ns]
                (cuilay/column
                 (map (fn [[root-ns-seg node]]
                        (ns-list-item {:selected-ns selected-ns :font-ui font-ui :fill-text fill-text} (assoc node :last-seg root-ns-seg)))
                      (sort-by key (ns-tree-map)))
                 #_(cuilay/padding
                    20 20
                    (text-editor/element editor))
                 (ui/gap 0 30))))))

(defn ns-interns-view []
  (cuilay/vscrollbar
   (cuilay/vscroll
    (ui/dynamic ctx [font-ui (:font-ui ctx)
                     fill-text (:fill-text ctx)
                     selected-ns @*selected-ns
                     selected-member @*selected-member]
                (cuilay/column
                 (for [avar (when selected-ns
                              (sort-by (comp name symbol) (vals (ns-interns selected-ns))))]
                   (cui/clickable
                    (uievt/on-primary-down (fn [_] (reset! *selected-member avar)))
                    (ui/fill
                     (doto (Paint.) (.setColor (unchecked-int (if (= avar selected-member)
                                                                0xFFC0FF90
                                                                0x00000000))))
                     (cuilay/padding
                      2 5
                      (ui/label (name (symbol avar)) font-ui fill-text)))))
                 #_(cuilay/padding
                    20 20
                    (text-editor/element editor))
                 (ui/gap 0 30))))))

(defn editor-view []
  (ui/dynamic
   ctx [member @*selected-member
        font-ui (:font-ui ctx)
        fill-text (:fill-text ctx)]
    (var-editor/var-panel member)
    #_(let [jar? (when-let [f (:file (meta member))]
                 (str/starts-with?
                  (str (.getResource (.getClassLoader (Class/forName "clojure.lang.Compiler")) f))
                  "jar"))]
     (if member
      (cuilay/padding
       5 5
       (if-let [ext-src (and #_jar? (clj.source/crude-source-of-var member))]
         (var-editor/var-panel member)
         #_(ui.error/bound-errors
          (text-editor/element (text-editor.core/make {:content ext-src :pos 0})))
         (ui/label "No source available" font-ui fill-text)))
      (ui/gap 0 0)))))

(defn basic-view []
  (cuilay/row
   [:stretch 1 (ns-list-view)]
   [:stretch 1 (ns-interns-view)]
   [:stretch 2 (editor-view)]))

(comment
  (repl/source-fn (symbol (first (vals (ns-interns *ns*)))))
  (repl/source-fn 'basic-view)
  (repl/source-fn 'chic.main/remount-app)
  (.getResourceAsStream (clojure.lang.RT/baseLoader) (:file (meta #'basic-view)))

  (require '[clojure.tools.analyzer.jvm :as ana.jvm])
  (def ana (ana.jvm/analyze-ns 'clojure.string))
  (map :op ana)
  ana.jvm/specials
  (do (ana.jvm/analyze-ns 'chic.cljbwr) nil)

  ;; TODO source path browser with indications as to which files and vars are loaded
  ;; static analysis is inevitable
  ;; only best way forward is to depend on clojure-lsp
  ;;   to use all the linting work already done.
  ;;     text based editing is inevitable
  ;;     but a safe editor can be built on top of text representation.
  ;;     and perhaps use library functions too.
  ;; Static analysis is mainly useful when code is not loaded
  ;; when code is loaded, a CIDER approach can be taken of using live objects
  ;;   maybe this is better - smalltalk like to encourage a constantly live system
  ;;   such that var defs are always eval-able
  ;;   and temporary/unfinished code goes in comments, separately.
  ;;   Thus, files remain 'clean' and the program is not broken.
  ;;   then the risk is that an imperative top-level form throws when the ns is required.
  ;;     static analysis needed to find the other vars.
  ;;      solutions:
  ;;        - use clj-kondo on the files that threw when initially required.
  ;;        - the top level forms can be found by parsing with rewrite-clj or just editing the plain file
  ;;           then the require can be retried. No intelligence of vars needed - just a debugging session.
  ;; Another problems:
  ;;   - files can be modified externally in another editor - invalidates :line :column var metadata. Needs static ana
  ;;   - refactoring would potentially be less reliable without static ana (namespaces that depend on a var may not be loaded)
  ;; Conclusion: static ana is essential to understand a project's dependency relationships and get reliable source code relationships.

  ;; More complex situation: var changes programmatically via alter-var-root
  (def x 0)
  (meta #'x)
  (alter-var-root #'x (fn [_] 1))
  ;; alter-var-root does not affect the metadata. :line remains the same.
  ;; may not be a huge problem as long as there is a single definite `def` and/or `declare` location
  ;; but indicates that you may want to be able to modify the def init code without evaluating it
  ;;   (as that would change the var's value)
  ;; but this is an edge case and probably only useful for things like function instrumentation.
  ;;   so may be more appropriate to allow hooks on the save-eval action so that modifications get reapplied
  ;;     this follows the more functional approach

  ;; so it still should be a valid approach to not save anything that has not been evaluated.
  ;; unsaved modifications remain open in a tab like in Morphic

  ;; browsers:
  ;; - source code (essentially initialisation code)
  ;; - live vars (that can have links to their init code)
  ;; this should be sufficient to bootstrap more specialised viewers down the line


  #!
  )
