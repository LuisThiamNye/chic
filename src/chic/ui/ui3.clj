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
   [clojure.tools.analyzer.ast :as ana.ast]
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

(defmacro fnlet-widget [fexpr]
  (let [input-syms (first (filter vector? fexpr))
        kebab->pascal (fn [s] (str/replace s #"(?:^|-)([a-z])"
                                           (fn [[_ l]] (str/upper-case l))))
        nsym (symbol (munge (kebab->pascal
                             (or (second (filter symbol? fexpr))
                                 (gensym "FnletWidget")))))
        expr-let (last fexpr)
        retexpr (last expr-let)
        method-ana (-> (binding [*warn-on-reflection* false]
                         (ana/analyze fexpr (assoc (ana/empty-env) :locals &env)))
                       :methods first)
        bindings-anas (-> method-ana :body :bindings)
        -visited-syms (java.util.HashSet.)
        bindings
        (mapv (fn [[sym vexpr] {:keys [tag init] :as ana}]
                (let [arg-deps (java.util.ArrayList.)
                      field-deps (java.util.ArrayList.)]
                  (ana.ast/prewalk
                   init (fn [a]
                          (when (= :arg (:local a))
                            (.add arg-deps (:form a)))
                          (when (.contains -visited-syms (:name a))
                            (.add field-deps (:form a)))
                          a))
                  (.add -visited-syms (:name ana))
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
        draw-param-vec (into (conj [(with-meta canvas-sym {:tag `Canvas})]
                                   (with-meta `change-mask## {:tag 'int}))
                             input-syms)
        existing-intf-cls (try (resolve i-sym)
                               (catch ClassNotFoundException _))
        unchanged-intf? (when existing-intf-cls
                          (= (mapv #(let [tag (:tag (meta %) 'java.lang.Object)
                                          r (resolve tag)]
                                      (if (var? r)
                                        tag r))
                                   draw-param-vec)
                             (mapv #(if (.isPrimitive ^Class %)
                                      (symbol (.getName ^Class %))
                                      %)
                                  (.getParameterTypes
                                   ^java.lang.reflect.Method
                                   (first (.getDeclaredMethods ^Class existing-intf-cls))))))
        expr-defint (when-not unchanged-intf?
                      `(definterface ~i-sym
                        (~'draw ~draw-param-vec)))
        draw-body (next (drop-while (complement vector?) (:draw retexpr)))
        field-vec (mapv (fn [{:keys [^Class tag sym]}]
                          (with-meta sym {:tag (symbol (.getName tag))
                                          :unsynchronized-mutable true})) bindings)]
    (pot/unify-gensyms
     `(do
        ~expr-defint
        (deftype ~nsym ~field-vec
          ~i-sym
          (draw ~(into ['_] (map #(with-meta % nil)) draw-param-vec)
            (when (< 0 change-mask##)
              (let [field-change-mask##
                    (-> 0
                        ~@(remove
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
                                                `(< 0 (bit-and change-mask## ~arg-deps-mask)))
                                    field-check (when (< 0 field-deps-mask)
                                                  `(< 0 (bit-and field-change-mask## ~field-deps-mask)))]
                                (when (or arg-check field-check)
                                  `(as-> field-change-mask##
                                     (cond-> field-change-mask##
                                       ~(if (and arg-check field-check)
                                          `(or ~arg-check ~field-check)
                                          (or arg-check field-check))
                                       (bit-or field-change-mask## (do (set! ~sym ~vexpr)
                                                                       ~(bit-shift-left 1 i))))))))
                            bindings)))]))
            ~@draw-body))
        {:java-draw-interface ~i-sym
         :java-class ~nsym
         ;; :classloader (.getContextClassLoader (Thread/currentThread))
         :input-syms '~input-syms}))))

(comment
  (->> (macroexpand-1 '(fnlet-widget (fn [] (let [x ^Image (+)] {:draw (fn [_])}))))
       (drop 2) first
       (drop 2) ffirst meta :tag type)

  (-> (ana/analyze '(fn [x] (let [y x
                                  z y] z)))
      :methods first :body :bindings
      second :local)
  (-> (ana/analyze '(fn [^int x]))
      :methods first :params
      first :tag type)
  (-> (ana/analyze '(fn [x] (let [y (+ x)] y)))
      :methods first :params first
      :arg-id)
  #!
  )
#_{:name _
   :fields [{:flags #{:public} :name _ :type _ :value nil}]
   :methods [{:flags #{:public} :name "draw" :desc [_ _ _]
              :emit (fn [^MethodVisitor mv])}]}

#_(binding [*compiler-options* {:disable-locals-clearing false}]
    (decompiler/disassemble (fn [] (println "Hello, decompiler!")))
    #_(decompiler/decompile (fn [] (+ 4))))
