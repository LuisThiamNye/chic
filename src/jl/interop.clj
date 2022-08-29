(ns jl.interop
  (:require
    [chic.util :as util])
  (:import
    (org.objectweb.asm Type)
    (java.lang.reflect Method Field Modifier)))

"Accessible https://docs.oracle.com/javase/specs/jls/se18/html/jls-6.html#jls-6.6
 and applicable https://docs.oracle.com/javase/specs/jls/se18/html/jls-15.html#jls-15.12.1"

(defn member-accessible? [^Class c1 ^Class target mods]
  (or (Modifier/isPublic mods)
    (let [pack2 (.getPackage target)
          pack1 (.getPackage c1)]
      (or (= pack2 pack1)
        (when (Modifier/isProtected mods)
          (.isAssignableFrom target c1))))))

(defn method-applicable? [arg-cs ^Class ret-c ^Method method]
  (and (or (nil? ret-c) (.isAssignableFrom (.getReturnType method) ret-c))
    (let [pts (.getParameterTypes method)]
      (or (and (= (count pts) (count arg-cs))
            (every? identity
              (map #(.isAssignableFrom %1 %2)
                pts arg-cs)))
          (when (.isVarArgs method)
            (let [vararg-type (last (.getParameterTypes method))]
              (and (every? identity
                     (map #(.isAssignableFrom %1 %2)
                       (butlast pts) arg-cs))
                (every? #(.isAssignableFrom vararg-type %)
                  (subvec arg-cs (dec (count (.getParameterTypes method))))))))))))

(defn match-method-type ^Type [classname static? method-name arg-types ret-type]
  (assert (every? some? arg-types))
  (let [c (Class/forName classname)
        ->c (fn [c] (or (try (util/primitive-name->class c)
                          (catch Exception _))
                      (Class/forName c)))
        arg-cs (mapv #(->c %) arg-types)
        ret-c (when ret-type (->c ret-type))
        matches (into [] (comp
                           (filter #(= (.getName ^Method %) method-name))
                           (filter #((if static? identity not)
                                     (Modifier/isStatic (.getModifiers %))))
                           (filter #(member-accessible? Object c (.getModifiers ^Method %)))
                           (filter #(method-applicable? arg-cs ret-c %))
                           )
                  (.getDeclaredMethods c))]
    (case (count matches)
      0 (throw (RuntimeException.
                 (str "No method matches: " classname (if static? "/" ".") method-name
                   " " (pr-str arg-types) " => " (pr-str ret-type))))
      1 (Type/getType ^Method (first matches))
      (throw (RuntimeException.
               (str "Method ambiguous: " classname (if static? "/" ".") method-name
                 " " (pr-str arg-types) " => " (pr-str ret-type)
                 "\nMatches: " (mapv #(.toString %) matches)))))))


(match-method-type "java.lang.String" "valueOf"
  ["int"] nil)
