(ns dev 
  (:require
    [riddley.walk :as rwalk]
    [chic.ui.ui3 :as ui3]
    [chic.debug.nrepl :refer [*last-eval*]]
    [user]
    [chic.debug :as debug :refer [*last-error*]]))


(do
  (def last-eval (let [le *last-eval*]
                 {:code (read-string (:code le))
                  :ns (find-ns (:ns le))}))
  (def last-code (:code last-eval)))

(debug/puget-prn
  last-code)

(binding [*ns* (:ns last-eval)
          *print-meta* true]
  (debug/puget-prn
    (rwalk/macroexpand-all last-code)))


(-> (:method-ana *last-error*)
  :body :body)

(-> *last-error*
  :method-ana
  :body :ret :body :ret)

(set! *print-length* 1000)
(set! *print-level* 7)






(count 
  (take-while identity (iterate #(.getParent %) 
                         (clojure.lang.RT/baseLoader))))

(def --cl (clojure.lang.DynamicClassLoader. (clojure.lang.RT/baseLoader)))

(def class-data
  {:name 'my.pkg.Adder
   :fields [{:flags #{:public :static}, :name "VALUE", :type :int, :value 42}]
   :methods [{:flags #{:public}, :name "add", :desc [:int :long]
              :emit [[:getstatic :this "VALUE" :int]
                     [:iload 1]
                     [:iadd]
                     [:i2l]
                     [:lreturn]]}]})

(require '[insn.core :as insn])

(def --x2 @(future
            (with-bindings {Compiler/LOADER --cl}
              (.setContextClassLoader (Thread/currentThread) --cl)
              (insn/define --cl class-data))))

(.getClassLoader --x)

(defn foo [x] (identity x))
(defn bar [y] (identity y))
(= (.getClassLoader (class foo)) (.getClassLoader (class bar)))

"
Classloaders:
Exception thrown if trying to define a new class if a classloader 
already had defined a class with the same name

Clojure repl uses a new classloader for each top level form.
NewInstanceExpr uses custom defineClass overload on DynamicClassLoader
which clears the class cache of unreachable classes.
Note the class cache is static, so only one Dynamic loader is sufficient.
Class cache contains classes defined with custom defineClass method.

Compiler/eval always creates a new classloader via RT/makeClassLoader

All evidence suggests references to the classloader and all its classes
must be gone for gc.

Dynamic dispatch things:
MethodHandle (faster reference to a method, with composability eg currying)
invokedynamic
http://blog.headius.com/2008/09/first-taste-of-invokedynamic.html
java.lang.ref - soft/weak references. Use a reference queue with them
to check for garbage collection.

Dynamic linking and call sites and constants etc:
java.dynalink
java.lang.invoke
  SwitchPoint - could eg be used for var dereffing. Call is specialised
   and you only pay the cost of dynamic dispatch when a new type is given
   to the call site, which re-specialises the site.
"

(deftype ReplClass [])
(def --x (defrecord ReplClass []))
(supers --x)