(ns chic.ui.ui3
  (:require
   [chic.ui.font :as uifont]
   [insn.core :as insn]
   [chic.debug]
   [chic.ui.interactor :as uii]
   [chic.style :as style]
   [io.github.humbleui.paint :as huipaint]
   [clojure.string :as str]
   [chic.util :as util]
   [chic.ui :as cui]
   [clojure.pprint :as pp]
   [chic.types :as types]
   [proteus :refer [let-mutable]]
   [potemkin :refer [doit] :as pot]
   [clojure.tools.analyzer.jvm :as ana]
   [clojure.tools.analyzer.passes :as ana.passes]
   [clojure.tools.analyzer.utils :as ana.util]
   [clojure.tools.analyzer.ast :as ana.ast]
   [clojure.tools.analyzer.passes.jvm.infer-tag :as ana.jvm.infer-tag]
   [clj-commons.primitive-math :as prim]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui])
  (:import
   (io.lacuna.bifurcan LinearList)
   (io.github.humbleui.skija.svg SVGLengthContext SVGDOM SVGSVG SVGLengthType)
   (java.util ArrayList HashMap)
   (io.github.humbleui.skija Data Canvas Font Paint TextLine FontMetrics)
   (io.github.humbleui.skija.shaper ShapingOptions Shaper)
   (io.github.humbleui.types IPoint IRect Rect Point)
   (io.github.humbleui.jwm EventMouseMove EventTextInput EventKey)
   (java.lang AutoCloseable)))

(def ^:dynamic *component-ctx* nil)

(def *class->cmpt (atom {}))

(defn class->cmpt [^Class cls]
  (@*class->cmpt (.getName cls)))

(defn class-default-value [^Class cls]
  (when (.isPrimitive cls)
    (case (.getName cls)
      "byte" (unchecked-byte 0)
      "short" (unchecked-short 0)
      "int" (unchecked-int 0)
      "long" (unchecked-long 0)
      "float" (unchecked-float 0)
      "double" (unchecked-double 0)
      "boolean" false
      "char" (unchecked-char 0))))

(defmacro fnlet-widget [fexpr]
  (let [raw-input-syms (first (filter vector? fexpr))
        input-syms (vec (sort raw-input-syms))
        kebab->pascal (fn [s] (str/replace s #"(?:^|-)([a-z])"
                                           (fn [[_ l]] (str/upper-case l))))
        nsym (symbol (munge (kebab->pascal
                             (or (second (filter symbol? fexpr))
                                 (gensym "FnletWidget")))))
        expr-let (macroexpand-1 (last fexpr))
        fexpr (concat (butlast fexpr) (list expr-let))
        retexpr (last expr-let)
        method-ana (-> (binding [*warn-on-reflection* false
                                 *component-ctx* {:fake? true}]
                         (ana/analyze fexpr (assoc (ana/empty-env) :locals &env)))
                       :methods first)
        bindings-anas (-> method-ana :body :bindings)
        -visited-syms (java.util.HashSet.)
        -visited-local-syms (java.util.HashSet.)
        bindings
        (mapv (fn [[sym vexpr] {:keys [tag init] :as ana}]
                (let [sym (if (.contains -visited-local-syms sym)
                            (gensym sym)
                            sym)
                      arg-deps (java.util.ArrayList.)
                      field-deps (java.util.ArrayList.)]
                  (ana.ast/prewalk
                   init (fn [a]
                          (cond
                            (= :arg (:local a))
                            (.add arg-deps (:form a))
                            (.contains -visited-syms (:name a))
                            (.add field-deps (:form a)))
                          a))
                  (.add -visited-syms (:name ana))
                  (.add -visited-local-syms sym)
                  {:tag tag :sym sym
                   :vexpr vexpr
                   :arg-deps (set arg-deps)
                   :field-deps (set field-deps)
                   :arg-deps-mask (reduce (fn [acc sym]
                                            (+ acc (bit-shift-left
                                                    1 (util/index-of input-syms sym))))
                                          0
                                          arg-deps)}))
              (eduction (partition-all 2) (second expr-let))
              bindings-anas)
        i-sym (symbol (str "I" nsym))
        canvas-sym (ffirst (filter vector? (:draw retexpr)))
        input-chmask-sym (gensym "input-chmask")
        draw-param-vec (into (conj [(with-meta canvas-sym {:tag `Canvas})]
                                   (with-meta input-chmask-sym {:tag 'int}))
                             ;; definterface needs fully qualified tag
                             (map (fn [{:keys [form tag]}]
                                    (with-meta form {:tag tag})))
                             (sort-by :form (-> method-ana :params)))
        existing-intf-cls (try (resolve i-sym)
                               (catch ClassNotFoundException _))
        unchanged-intf? (when existing-intf-cls
                          (= (mapv #(let [tag (:tag (meta %) Object)]
                                      (if (symbol? tag)
                                        (str tag) (.getName ^Class tag)))
                                   draw-param-vec)
                             (mapv #(.getName ^Class %)
                                   (try (.getParameterTypes
                                         ^java.lang.reflect.Method
                                         (first (.getDeclaredMethods ^Class existing-intf-cls)))
                                        (catch NoClassDefFoundError _
                                          ;; When existing interface is invalid
                                          nil)))))
        expr-defint (when-not unchanged-intf?
                      `(definterface ~i-sym
                         (~'draw ~draw-param-vec)))
        draw-body (next (drop-while (complement vector?) (:draw retexpr)))
        field-vec (mapv (fn [{:keys [^Class tag sym]}]
                          (with-meta sym {:tag (symbol (.getName tag))
                                          :unsynchronized-mutable true})) bindings)
        field-chmask-sym (gensym "field-chmask")
        body-ana (-> method-ana :body :body)
        fch-conds (remove
                   nil?
                   (map-indexed
                    (fn [i {:keys [sym vexpr field-deps arg-deps-mask]}]
                      (let [field-deps-mask
                            (reduce + (remove nil?
                                              (map-indexed
                                               (fn [i {:keys [sym]}]
                                                 (when (contains? field-deps sym) i))
                                               bindings)))
                            arg-check (when (< 0 arg-deps-mask)
                                        `(< 0 (bit-and ~input-chmask-sym ~arg-deps-mask)))
                            field-check (when (< 0 field-deps-mask)
                                          `(< 0 (bit-and ~field-chmask-sym ~field-deps-mask)))]
                        (when (or arg-check field-check)
                          `(as-> ~field-chmask-sym
                                 (cond-> ~field-chmask-sym
                                   ~(if (and arg-check field-check)
                                      `(or ~arg-check ~field-check)
                                      (or arg-check field-check))
                                   (do (set! ~sym ~vexpr)
                                       (bit-set ~field-chmask-sym ~i)))))))
                    bindings))]
    `(do
       ~expr-defint
       (eval
        '(do
           (deftype ~nsym ~field-vec
             ~i-sym
             (draw ~(into ['_] (map #(with-meta % nil)) draw-param-vec)
               (let [~field-chmask-sym
                     ~(if (empty? fch-conds)
                        0
                        `(cond-> 0
                           (< 0 ~input-chmask-sym)
                           (-> ~@fch-conds)))]
                 ~(binding
                   [*component-ctx* {:raw-body-ana body-ana
                                     :inputs (vec (map-indexed (fn [i sym] {:sym sym
                                                                            :arg-id i})
                                                               raw-input-syms))
                                     :fields (mapv (fn [ana] {:ana-name (:name ana)
                                                              :sym (:form ana)})
                                                   bindings-anas)
                                     :input-chmask-sym input-chmask-sym
                                     :field-chmask-sym field-chmask-sym}]
                    (ana/macroexpand-all `(do ~@draw-body)
                                         (-> body-ana :env))))))
           (let [ret# {:java-draw-interface (resolve '~i-sym)
                       :java-class (resolve '~nsym)
                       ;; :classloader (.getContextClassLoader (Thread/currentThread))
                       :constructor
                       (fn [] (new ~nsym ~@(map (fn [{:keys [arg-deps field-deps vexpr tag]}]
                                                  (if (and (empty? arg-deps) (empty? field-deps))
                                                    vexpr
                                                    (class-default-value tag)))
                                                bindings)))
                       :input-syms '~input-syms}]
             (swap! *class->cmpt assoc
                    (.getName ^Class (resolve '~i-sym)) ret#)
             ret#))))))

(defmacro new-cmpt [cmpt-sym]
  (let [cmpt @(resolve cmpt-sym)]
    (with-meta `((:constructor ~cmpt-sym))
      {:tag (symbol (.getName ^Class (:java-draw-interface cmpt)))
       #_(:java-draw-interface cmpt)})))

(defn draw-cmpt* [{:keys [raw-body-ana inputs fields
                          input-chmask-sym field-chmask-sym]}
                  cmpt canvas-sym cmpt-sym argmap]
  (let [param-syms (:input-syms cmpt)
        argmap-ana (let [*ret (volatile! nil)]
                     (ana.ast/prewalk raw-body-ana
                                      (fn [node]
                                        (when (= argmap (:form node))
                                          (chic.debug/println-main "ok"))
                                        (if (identical? argmap (:form node))
                                          (reduced (vreset! *ret node))
                                          node)))
                     @*ret)
        _ (assert (some? argmap-ana))
        input-arg-ids (set (map :arg-id inputs))
        field-ana-names (set (map :ana-name fields))
        args
        (mapv (fn [sym]
                (let [argmap-idx (some identity
                                       (map-indexed
                                        (fn [i kana]
                                          (when (= (keyword sym) (:val kana)) i))
                                        (-> argmap-ana :keys)))
                      expr-ana (-> argmap-ana :vals (nth argmap-idx))
                      arg-deps (java.util.ArrayList.)
                      field-deps (java.util.ArrayList.)
                      _ (ana.ast/prewalk
                         expr-ana (fn [a]
                                    (cond
                                      (contains? input-arg-ids (:arg-id a))
                                      (.add arg-deps (:form a))
                                      (contains? field-ana-names (:name a))
                                      (.add field-deps (:form a)))
                                    a))
                      input-deps (set arg-deps)
                      field-deps (set field-deps)
                      make-mask (fn [deps variables]
                                  (reduce + (map (fn [i]
                                                   (let [sym (:sym (nth variables i))]
                                                     (if (contains? deps sym)
                                                       (bit-shift-left 1 i) 0)))
                                                 (range (count variables)))))]
                  {:expr (argmap (keyword sym))
                   :sym sym
                   :input-deps input-deps
                   :field-deps field-deps
                   :input-depmask (make-mask input-deps inputs)
                   :field-depmask (make-mask field-deps fields)}))
              param-syms)
        chmask-expr
        `(-> ~(unchecked-int 0)
             ~@(remove
                nil? (map-indexed
                      (fn [i {:keys [input-depmask field-depmask]}]
                        (let [depmask-expr
                              (reduce (fn ([])
                                        ([acc expr]
                                         `(bit-and ~acc ~expr)))
                                      (remove nil? (map (fn [[sym mask]]
                                                          (when (< 0 mask)
                                                            `(bit-and ~sym ~mask)))
                                                        [[input-chmask-sym input-depmask]
                                                         [field-chmask-sym field-depmask]])))]
                          (when depmask-expr
                            `(cond-> (< 0 ~depmask-expr)
                               (bit-set ~i)))))
                      args)))]
    `(.draw ~cmpt-sym ~canvas-sym ~chmask-expr ~@(map :expr args))))
(comment
  (do (def --argmap '{:c 1 :b x :a (+ x y 4)})
      (def --argmapana (ana/analyze `(let [~'x 10 ~'y 20]
                                       ~--argmap))))
  (let [x 5]
    (util/compile
     (fn [&env]
       (type (&env 'x)))))

  (-> --argmapana
      :body map?)
  (reduce bit-and [])

  (chic.debug/println-main
   (zprint.core/zprint-str
    (draw-cmpt* {:form-ana {:args ['... (-> --argmapana :body)]}
                 :inputs []
                 :fields []
                 :input-chmask-sym 'input-chmask
                 :field-chmask-sym 'field-chmask}
                {:input-syms ['a 'b 'c]} 'cnv 'cmpt --argmap)))

  (clojure.tools.analyzer.passes.jvm.emit-form/emit-form
   (ana.ast/postwalk
    (ana/analyze '(let [a 9]
                    (into 1 2 3)))
    (fn [ana]
      (if (= #'into (:var (:fn ana)))
        (assoc ana :raw-forms '((+ 3)))
        ana))))

  (-> (ana/analyze '(let [a 9]
                      (transduce 1 2 -)))
      :body :form)
  (let [code '(let [a 9]
                (into 1 2 3))
        *out (atom nil)]
    (ana.ast/postwalk
     (ana/analyze code)
     (fn [ana]
       (if (identical? (:form ana) (last code))
         (reset! *out ana)
         ana)))
    (-> @*out
        :op))

  #!
  )

(defn env-binding->tag ^Class [binding]
  (let [tag (or (:tag binding)
                (if (map? binding)
                  (:tag ((ana.passes/schedule
                          #{#'ana.jvm.infer-tag/infer-tag})
                         binding))
                  (let [binding ^clojure.lang.Compiler$LocalBinding binding]
                    (when (.hasJavaClass binding) (.getJavaClass binding)))))]
    (if (symbol? tag)
      (resolve tag)
      tag)))

(defn resolve-cmpt [sym env]
  (class->cmpt (env-binding->tag (env sym))))

(defmacro draw-cmpt [cnv-sym cmpt-sym argmap]
  (if (:fake? *component-ctx*)
    `(do ~argmap nil)
    (let [cmpt (resolve-cmpt cmpt-sym &env)]
      (draw-cmpt* *component-ctx* cmpt
                  cnv-sym cmpt-sym argmap))))

(defn draw-cmpt-external*
  [cmpt cnv cmpt-sym chmask-expr argvec argmap]
  (let [argmap (reduce (fn [m sym]
                         (let [k (keyword sym)]
                           (if (contains? m k)
                             m
                             (assoc m k sym))))
                       argmap
                       argvec)]
    `(.draw ~cmpt-sym ~cnv ~chmask-expr
            ~@(map (fn [sym]
                     (argmap (keyword sym)))
                   (:input-syms cmpt)))))

(defmacro draw-cmpt-ext [cmpt-sym cnv chmask & [a1 a2]]
  (let [argvec (if (vector? a1) a1 [])
        argmap (or a2 a1)
        argmap (if (map? argmap) argmap {})
        cmpt (resolve-cmpt cmpt-sym &env)]
    (draw-cmpt-external* cmpt cnv cmpt-sym chmask argvec argmap)))

#_(defn free-variables [expr]
    (let [*set (proteus.Containers$O. (transient #{}))]
      (ana/analyze expr (ana/empty-env)
                   {:passes-opts
                    (assoc ana/default-passes-opts
                           :validate/unresolvable-symbol-handler
                           (fn [_ns namesym _ast]
                             (.set *set (conj! (.-x *set) namesym))
                             {:op :const
                              :env {}
                              :type :nil
                              :literal? true
                              :val nil
                              :form nil
                              :top-level true
                              :o-tag nil
                              :tag nil}))})
      (persistent! (.-x *set))))

(defn localbinding->map [obj]
  (if (map? obj)
    obj
    (let [obj ^clojure.lang.Compiler$LocalBinding obj]
      {:op :binding
       :name (.-sym obj)
       :form (.-sym obj)
       :local (if (.-isArg obj) :arg :let)
       :tag (when (.hasJavaClass obj) (.getJavaClass obj))
      ;; :init {}
       :children [] #_[:init]})))

(defmacro cmpt-ext-input-memory [cmpt-sym]
  (let [cmpt (resolve-cmpt cmpt-sym &env)]
    `(object-array ~(count (:input-syms cmpt)))))

;; ^"[Ljava.lang.Object;"
(defmacro draw-cmpt-ext-memo [cmpt-sym cnv argary argmap]
  (util/let-macro-syms [argary argary]
    (let [cmpt (resolve-cmpt cmpt-sym &env)
          input-syms (:input-syms cmpt)
          inputs (map-indexed
                  (fn [i sym]
                    (let [vexpr (argmap (keyword sym))]
                      (if (= :const (:op (ana/analyze
                                          vexpr (assoc (ana/empty-env)
                                                       :locals (update-vals &env localbinding->map)))))
                        {:const? true :i i}
                        {:i i :sym sym :val-sym (if (symbol? vexpr)
                                                  vexpr
                                                  (gensym sym))
                         :vexpr vexpr
                         :symbol? (symbol? vexpr)})))
                  input-syms)
          init-idx (:i (first (filter :const? inputs)))
          variable-inputs (remove :const? inputs)
          varexpr-inputs (remove :symbol? variable-inputs)
          argmap (reduce (fn [m {:keys [sym val-sym]}]
                           (assoc m (keyword sym) val-sym))
                         argmap varexpr-inputs)
          chmask-bindings (mapcat (fn [{:keys [val-sym i]}]
                                    `[chmask## (if (identical? ~val-sym (aget ~argary ~i))
                                                 chmask##
                                                 (do (aset ~argary ~i ~val-sym)
                                                     (bit-set chmask## ~i)))])
                                  variable-inputs)]
      `(let [~@(mapcat (fn [{:keys [vexpr val-sym]}]
                         [val-sym vexpr])
                       varexpr-inputs)
             chmask## ~(if init-idx
                         `(if (nil? (aget ~argary ~init-idx))
                            (do (aset ~argary ~init-idx true)
                                Integer/MAX_VALUE)
                            (let [chmask## ~(unchecked-int 0)
                                  ~@chmask-bindings]
                              chmask##))
                         ~(unchecked-int 0))
             ~@(when-not init-idx chmask-bindings)]
         ~(draw-cmpt-external* cmpt cnv cmpt-sym `chmask## [] argmap)))))

(comment
  (->> (macroexpand-1 '(fnlet-widget (fn [] (let [x ^Image (+)] {:draw (fn [_])}))))
       (drop 2) first
       (drop 2) ffirst meta :tag type)

  (->> (macroexpand-1 '(fnlet-widget (fn [] (let [{:keys [x y]} (+)] {:draw (fn [_])}))))
       (drop 2) first
       (drop 2) ffirst meta :tag type)
  (macroexpand-1 '(let [{:keys [x y]} (+)] {:draw (fn [_])}))

  (-> (ana/analyze '(fn [x] (let [z x
                                  z z] z)))
      :methods first :body :bindings
      first keys)
  (-> (ana/analyze '(fn [^int x]))
      :methods first :params
      first :tag type)
  (-> (ana/analyze '(fn [x] (let [y (+ x)] y)))
      :methods first :params first
      :arg-id)

  (eval
   (ana/macroexpand-all
    (util/quoted
     (let [x []
           x []]
       (util/compile
        (fn [&env]
          `(quote
            ~(do (&env 'x)))))))))
  (let [x []]
    (util/compile
     (fn [&env]
       `(quote
         ~(do (.-sym (&env 'x)))))))
  #!
  )
#_{:name _
   :fields [{:flags #{:public} :name _ :type _ :value nil}]
   :methods [{:flags #{:public} :name "draw" :desc [_ _ _]
              :emit (fn [^MethodVisitor mv])}]}

#_(binding [*compiler-options* {:disable-locals-clearing false}]
    (decompiler/disassemble (fn [] (println "Hello, decompiler!")))
    #_(decompiler/decompile (fn [] (+ 4))))
