(ns chic.ui.ui3
  (:require
   [chic.ui.font :as uifont]
   [insn.core :as insn]
   [chic.debug :as debug]
   [chic.ui.interactor :as uii]
   [chic.style :as style]
   [io.github.humbleui.paint :as huipaint]
   [clojure.string :as str]
   [chic.util :as util]
   [chic.ui :as cui]
   [clojure.pprint :as pp]
   [chic.windows :as windows]
   [chic.types :as types]
   [proteus :refer [let-mutable]]
   [potemkin :refer [doit] :as pot]
   [clojure.tools.analyzer.jvm :as ana]
   [clojure.tools.analyzer.passes :as ana.passes]
   [clojure.tools.analyzer.utils :as ana.util]
   [clojure.tools.analyzer.ast :as ana.ast]
   [clojure.tools.analyzer.passes.jvm.infer-tag :as ana.jvm.infer-tag]
   [clojure.tools.analyzer.passes.jvm.constant-lifter :as anap.constant-lifter]
   [clj-commons.primitive-math :as prim]
   [io.github.humbleui.core :as hui :refer [deftype+]]
   [io.github.humbleui.protocols :as huip :refer [IComponent]]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui]
   [taoensso.encore :as enc])
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

(def ^Class cmpt3-java-interface
  (insn/define {:flags #{:public :interface :abstract}
                :name (util/expand-class-sym 'ICmpt3)
                :interfaces ['java.lang.AutoCloseable]
                :version 19
                :methods [{:flags #{:public} :name 'close :desc [:void]
                           :emit [[:return]]}]}))

(defn fnlet-widget-to-code*
  [{:keys [bindings input-chmask-sym c-sym bindings-anas
           unchanged-intf? i-sym body-ana draw-param-vec
           raw-input-syms input-syms retexpr]}]
  (assert (<= (count input-syms) 32))
  (assert (<= (count bindings) 32))
  (let [draw-body (next (drop-while (complement vector?) (:draw retexpr)))
        used-bindings (filterv (complement :unused?) bindings)
        field-vec (mapv (fn [{:keys [^Class tag sym independent?]}]
                          (with-meta sym (cond-> {:tag (symbol (.getName tag))}
                                           (not independent?)
                                           (assoc :unsynchronized-mutable true))))
                        used-bindings)
        field-chmask-sym (gensym "field-chmask")
        fch-conds (remove
                   nil?
                   (map-indexed
                    (fn [i {:keys [sym vexpr input-depmask field-depmask unused?]}]
                      (let [arg-check (when-not (== 0 input-depmask)
                                        `(not (== 0 (bit-and ~input-chmask-sym ~input-depmask))))
                            field-check (when-not (== 0 field-depmask)
                                          `(not (== 0 (bit-and ~field-chmask-sym ~field-depmask))))]
                        (when (or arg-check field-check)
                          `(as-> ~field-chmask-sym
                                 (cond-> ~field-chmask-sym
                                   ~(if (and arg-check field-check)
                                      `(or ~arg-check ~field-check)
                                      (or arg-check field-check))
                                   ~(if unused?
                                      `(do ~vexpr ~field-chmask-sym)
                                      `(do (set! ~sym ~vexpr)
                                           (bit-set ~field-chmask-sym ~i))))))))
                    bindings))
        closeable-field-syms
        (mapv :sym (filter (fn [{:keys [tag]}]
                             (isa? tag java.lang.AutoCloseable)) bindings))]
    `(binding [*unchecked-math* true]
       (let [iface# ~(if unchanged-intf?
                       `(resolve '~i-sym)
                       `(insn/define
                          {:flags #{:public :interface :abstract}
                           :name '~i-sym
                           :interfaces [cmpt3-java-interface]
                           :version 19
                           :methods [{:flags #{:public :abstract} :name "draw"
                                      :desc ~(conj (mapv (comp :tag meta) draw-param-vec)
                                                   :void)}]}))
             cls# (eval
                   (quote
                    (deftype ~c-sym ~field-vec
                      ~i-sym
                      (draw ~(into ['_] (map #(with-meta % nil)) draw-param-vec)
                        (let [~field-chmask-sym
                              ~(if (empty? fch-conds)
                                 (int 0)
                                 `(cond-> ~(int 0)
                                    (not (== 0 ~input-chmask-sym))
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
                                                  (-> body-ana :env)))))
                      ~@(when closeable-field-syms
                          `[java.lang.AutoCloseable
                            (close [_#]
                                   ~@(mapv (fn [sym] `(.close ~sym))
                                           closeable-field-syms))]))))]
         (let [ret# {:java-draw-interface iface#
                     :java-class cls#
                     :constructor
                     (eval '(fn [] (new ~c-sym ~@(map (fn [{:keys [arg-deps field-deps vexpr tag]}]
                                                        (if (and (empty? arg-deps) (empty? field-deps))
                                                          vexpr
                                                          (class-default-value tag)))
                                                      used-bindings))))
                     :input-syms '~input-syms}]
           (swap! *class->cmpt assoc
                  (.getName ^Class (resolve '~i-sym)) ret#)
           ret#)))))

(defn existing-draw-iface-sig* [^Class existing-iface]
  (let [method ^java.lang.reflect.Method
        (some #(when (= "draw" (.getName ^java.lang.reflect.Method %)) %)
              (try (.getDeclaredMethods existing-iface)
                (catch NoClassDefFoundError _
                  ;; When existing interface is invalid
                  nil)))]
    (mapv (fn [^Class c] (if (.isPrimitive c)
                           (.getName c) c))
          (.getParameterTypes method))))

(defn param-vec->sig* [pv]
  (mapv #(let [tag (:tag (meta %) Object)
               typ (if (or (symbol? tag) (string? tag))
                     (let [r (resolve (symbol tag))]
                       (if (class? r) r (str tag)))
                     tag)]
           (if (class? typ)
             (let [c ^Class typ]
               (if (.isPrimitive c) (.getName c) c))
             typ))
        pv))

(defn fnlet-widget-parse* [fexpr & [&env]]
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
        (mapv (fn [[specified-sym vexpr] {:keys [tag init] :as ana}]
                (let [sym (if (.contains -visited-local-syms specified-sym)
                            (gensym specified-sym)
                            specified-sym)
                      input-deps (java.util.ArrayList.)
                      field-deps (java.util.ArrayList.)
                      input-ana-names (set (map :name (:params method-ana)))
                      _ (ana.ast/prewalk
                         init (fn [a]
                                (cond
                                  (contains? input-ana-names (:name a))
                                  (.add input-deps (:form a))
                                  (.contains -visited-syms (:name a))
                                  (.add field-deps (:form a)))
                                a))
                      field-deps (set field-deps)
                      input-deps (set input-deps)]
                  (.add -visited-syms (:name ana))
                  (.add -visited-local-syms sym)
                  {:tag tag :sym sym
                   :vexpr vexpr
                   :independent? (and (empty? input-deps) (empty? field-deps))
                   :unused? (= '_ specified-sym)
                   :arg-deps input-deps
                   :field-deps field-deps
                   :input-depmask
                   (reduce (fn [acc input-sym]
                             (bit-set acc (util/index-of input-syms input-sym)))
                           0 input-deps)
                   :field-depmask
                   (enc/reduce-indexed (fn [acc i {:keys [form]}]
                                         (cond-> acc (contains? field-deps form)
                                                 (bit-set i)))
                                       0 bindings-anas)}))
              (eduction (partition-all 2) (second expr-let))
              bindings-anas)
        i-sym (util/expand-class-sym (symbol (str "I" nsym)))
        canvas-sym (ffirst (filter vector? (:draw retexpr)))
        input-chmask-sym (gensym "input-chmask")
        draw-param-vec (into (conj [(with-meta canvas-sym {:tag `Canvas})]
                                   (with-meta input-chmask-sym {:tag 'int}))
                             (map (fn [{:keys [form tag]}]
                                    (with-meta form {:tag tag})))
                             (sort-by :form (-> method-ana :params)))
        existing-iface (try (resolve i-sym)
                            (catch ClassNotFoundException _))
        unchanged-intf? (when (and existing-iface
                                   (isa? existing-iface cmpt3-java-interface))
                          (= (param-vec->sig* draw-param-vec)
                             (existing-draw-iface-sig* existing-iface)))
        body-ana (-> method-ana :body :body)]
    {:body-ana body-ana
     :c-sym nsym
     :bindings-anas bindings-anas
     :retexpr retexpr
     :bindings bindings
     :raw-input-syms raw-input-syms
     :input-syms input-syms
     :unchanged-intf? unchanged-intf?
     :existing-iface existing-iface
     :i-sym i-sym
     :input-chmask-sym input-chmask-sym
     :draw-param-vec draw-param-vec}))

(defmacro fnlet-widget [fexpr]
  (fnlet-widget-to-code* (fnlet-widget-parse* fexpr &env)))

(defn draw-cmpt->code* [])

(defn draw-cmpt* [{:keys [raw-body-ana inputs fields
                          input-chmask-sym field-chmask-sym] :as ctx}
                  cmpt canvas-sym cmpt-expr argmap]
  (let [param-syms (:input-syms cmpt)
        [cmpt-expr-ana
         argmap-ana] (let [*ret (volatile! nil)
                         *cmpt-ana (volatile! nil)]
                     (ana.ast/prewalk raw-body-ana
                                      (fn [{:keys [form] :as node}]
                                        (cond-> (cond
                                                  (identical? argmap form)
                                                  (vreset! *ret node)
                                                  (identical? cmpt-expr form)
                                                  (vreset! *cmpt-ana node)
                                                  :else node)
                                          (and @*ret @*cmpt-ana) reduced)))
                     [@*cmpt-ana @*ret])
        _ (assert (some? argmap-ana))
        ;; _ (assert (some? cmpt-expr-ana))
        spc-init-expr (:-init? argmap)
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
                      input-deps (java.util.ArrayList.)
                      field-deps (java.util.ArrayList.)
                      process-subexpr-ana
                      (fn [a]
                        (cond
                          (contains? input-arg-ids (:arg-id a))
                          (.add input-deps (:form a))
                          (contains? field-ana-names (:name a))
                          (.add field-deps (:form a)))
                        a)
                      const? (:const (:op expr-ana))
                      _ (ana.ast/prewalk expr-ana process-subexpr-ana)
                      ;; _ (when (and (empty? input-deps) (empty? field-deps))
                      ;;     (process-subexpr-ana cmpt-expr-ana))
                      input-deps (set input-deps)
                      field-deps (set field-deps)
                      make-mask (fn [deps variables]
                                  (reduce + (map (fn [i]
                                                   (let [sym (:sym (nth variables i))]
                                                     (if (contains? deps sym)
                                                       (bit-shift-left 1 i) 0)))
                                                 (range (count variables)))))]
                  {:expr (argmap (keyword sym))
                   :expr-ana expr-ana
                   :const? const?
                   :sym sym
                   :input-deps input-deps
                   :field-deps field-deps
                   :input-depmask (make-mask input-deps inputs)
                   :field-depmask (make-mask field-deps fields)}))
              param-syms)
        chmask-conds
        `(-> ~(int 0)
             ~@(eduction
                (map-indexed
                 (fn [i {:keys [input-depmask field-depmask] :as arg}]
                   (let [depmask-expr
                         (reduce (fn ([])
                                   ([acc expr]
                                    `(bit-or ~acc ~expr)))
                                 (sequence
                                  (comp (map (fn [[sym mask]]
                                               (when-not (== 0 mask)
                                                 `(bit-and ~sym ~mask))))
                                        (remove nil?))
                                  [[input-chmask-sym input-depmask]
                                   [field-chmask-sym field-depmask]]))]
                     (when depmask-expr
                       `(cond-> (not (== 0 ~depmask-expr))
                          (bit-set ~i))))))
                (remove nil?)
                args))
        chmask-expr
        (if spc-init-expr
          `(if ~spc-init-expr
             Integer/MAX_VALUE
             ~chmask-conds)
          chmask-conds)]
    (debug/report-data :draw-cmpt {:args args
                                   :cmpt-expr-ana cmpt-expr-ana
                                   :field-ana-names field-ana-names
                                   :ctx ctx
                                   :cmpt-expr cmpt-expr})
    #_(when (nil? cmpt-expr-ana)
      (throw (ex-info "Could not find cmpt-expr analysis ast" {})))
    (when-not spc-init-expr
      (doit [{:keys [input-depmask field-depmask sym]} args]
       (when (== 0 input-depmask field-depmask)
         (throw (ex-info (format "Arg for %s cannot be independent" sym) {})))))
    `(.draw ~(with-meta cmpt-expr {:tag (symbol (.getName ^Class (:java-draw-interface cmpt)))})
            ~canvas-sym ~chmask-expr ~@(map :expr args))))

(defn env-binding->tag ^Class [binding]
  (let [tag (util/local-binding-tag binding)]
    (if (symbol? tag)
      (resolve tag)
      tag)))

(defn resolve-cmpt [expr env]
  (or (let [sym (:cmpt (meta expr))
            r (when (symbol? sym)
                (resolve sym))]
        (when (var? r)
          @r))
      (when (symbol? expr)
        (class->cmpt (env-binding->tag (env expr))))
      (throw (ex-info (str "Could not resolve component: " expr) {}))))

(defmacro draw-cmpt [cmpt-expr cnv-sym argmap]
  (let [ctx *component-ctx*]
    (if (:fake? ctx)
     `(do ~cmpt-expr ~argmap nil)
     (let [cmpt (resolve-cmpt cmpt-expr &env)]
       (draw-cmpt* ctx cmpt cnv-sym cmpt-expr argmap)))))

(defn select-from-mask* [mask coll]
  (into #{} (comp (map-indexed
                   (fn [i x] (when (bit-test mask i) x)))
                  (remove nil?))
        coll))

(defmacro get-changed-input-syms []
  (let [ctx *component-ctx*]
    (when-not (:fake? ctx)
     `(select-from-mask* ~(:input-chmask-sym ctx)
                         '~(mapv :sym (:inputs ctx))))))

(defmacro get-changed-field-syms []
  (let [ctx *component-ctx*]
    (when-not (:fake? ctx)
      `(select-from-mask* ~(:field-chmask-sym ctx)
                          '~(mapv :sym (:fields ctx))))))

(defmacro new-cmpt [cmpt-sym]
  (let [cmpt @(resolve cmpt-sym)]
    (with-meta `((:constructor ~cmpt-sym))
      {:tag (symbol (.getName ^Class (:java-draw-interface cmpt)))})))

(defn draw-cmpt-external*
  [cmpt cnv cmpt-sym chmask-expr argvec argmap]
  (let [argmap (reduce (fn [m sym]
                         (let [k (keyword sym)]
                           (if (contains? m k)
                             m
                             (assoc m k sym))))
                       argmap
                       argvec)
        param-syms (:input-syms cmpt)]
    (assert (= (set param-syms)
               (into (set argvec) (map symbol) (keys argmap)))
            "All component parameters must be specified exactly")
    `(.draw ~cmpt-sym ~cnv ~chmask-expr
            ~@(map (fn [sym]
                     (argmap (keyword sym)))
                   param-syms))))

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

(defmacro cmpt-ext-input-memory [cmpt-sym]
  (let [cmpt (resolve-cmpt cmpt-sym &env)]
    `(object-array ~(count (:input-syms cmpt)))))

(defmacro draw-cmpt-ext-memo [cmpt-sym cnv argary argmap]
  (util/let-macro-syms [argary argary]
    (let [cmpt (resolve-cmpt cmpt-sym &env)
          input-syms (:input-syms cmpt)
          inputs
          (map-indexed
           (fn [i sym]
             (let [vexpr (argmap (keyword sym))]
               (if (util/analyze-const? vexpr &env)
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
          chmask-bindings
          (mapcat (fn [{:keys [val-sym i]}]
                    `[chmask## (if (identical? ~val-sym (aget ~argary ~(unchecked-int i)))
                                 chmask##
                                 (do (aset ~argary ~(unchecked-int i) ~val-sym)
                                     (bit-set chmask## ~i)))])
                  variable-inputs)]
      `(let [~@(mapcat (fn [{:keys [vexpr val-sym]}]
                         [val-sym vexpr])
                       varexpr-inputs)
             chmask## ~(if init-idx
                         `(if (nil? (aget ~argary ~init-idx))
                            (do (aset ~argary ~init-idx true)
                                Integer/MAX_VALUE)
                            ~(unchecked-int 0))
                         (unchecked-int 0))
             ~@chmask-bindings]
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

(defn get-mouse-pos []
  (:chic.ui/mouse-win-pos (.get windows/*root-ctx)))
