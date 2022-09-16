(ns jl.rv-data
  (:require
    [chic.util :refer [<-]]
    [clojure.walk :as walk]
    [lambdaisland.regal :as regal]
    [jl.reader :as reader :refer [PFormVisitor]]))

(defn re-group [re ^java.util.regex.MatchResult match group]
  (let [idx (get (:groups re) group)]
    (.group match idx)))

(defn make-regex- [forms]
  (let [*groups (volatile! {})
        *n (volatile! 1)
        forms' (walk/postwalk 
                 (fn [form]
                   (if (and (vector? form) (< 0 (count form)))
                     (let [k (nth form 0)]
                       (condp = k
                         :capture (do (vswap! *n inc) form)
                         :capture' (let [n @*n]
                                     (vreset! *n (inc n))
                                     (vswap! *groups assoc (nth form 1) n)
                                     (into [:capture] (subvec form 2)))
                         form))
                     form))
                 forms)]
    {:groups @*groups
     :regex (regal/regex forms')}))

(def int-re
  (make-regex-
    [:cat
     [:? [:capture' :sign [:class \+ \-]]]
     [:alt
      [:cat [:alt
             [:cat [:capture' :leading-zero "0"]
              [:alt [:capture' :radix-letter [:class \x \b]]
               [:lookahead :digit]]]
             [:cat [:capture' :radix [:+ :digit]]
              "r"]]
       [:capture' :number-radix [:+ [:class [\A \Z] [\a \z] [\0 \9]]]]]
      [:capture' :number [:+ :digit]]]
     [:? [:capture' :type
          [:class \b \i \L \N]]]]))

(def float-re
  (make-regex-
    [:cat
     [:? [:capture' :sign [:class \+ \-]]]
     [:alt
      [:capture' :number
       [:cat [:+ :digit] [:? [:cat \. [:* :digit]]]]]
      [:cat
       [:capture' :percentage
        [:cat [:+ :digit] [:? [:cat \. [:* :digit]]]]]
       \%]]
     [:? [:capture' :type
          [:class \f \d \M]]]]))

(defn parse-number [num]
  (<-
    (let [m (.matcher ^java.util.regex.Pattern (:regex int-re) num)])
    (if (.matches m)
      (let [sign (re-group int-re m :sign)
            neg (= "-" sign)
            radix (if (nil? (re-group int-re m :leading-zero))
                    (if-some [r (re-group int-re m :radix)]
                      (Integer/parseInt r)
                      10)
                    (if-some [r (re-group int-re m :radix-letter)]
                      (cond
                        (= r "x") 16
                        (= r "b") 2)
                      8))
            unsigned? (not (or (= 10 radix) sign))
            number (or (re-group int-re m :number)
                     (re-group int-re m :number-radix))
            typ (re-group int-re m :type)
            typ (cond
                  (= "b" typ) :byte
                  (= "i" typ) :int
                  (= "L" typ) :long
                  (= "N" typ) :bigint
                  :else :none)]
        {:neg neg :type typ :number number :kind :int :radix radix
         :unsigned? unsigned?}))
    (let [m (.matcher (:regex float-re) num)])
    (if (.matches m)
      (let [sign (re-group float-re m :sign)
            neg (= "-" sign)
            number (re-group float-re m :number)
            number' (or number (re-group float-re m :percentage))
            typ (re-group float-re m :type)
            typ (cond
                  (= "f" typ) :float
                  (= "d" typ) :double
                  (= "M" typ) :bigdec
                  :else :none)]
        {:neg neg :type typ :number number' :kind :float :percent? (nil? number)}))
    nil))

(comment
  (parse-number "12")
  (parse-number "12")
  (parse-number "+12")
  (parse-number "012")
  (parse-number "0x12")
  (parse-number "0xFFi")
  (parse-number "0b10")
  (parse-number "0b10N")
  (parse-number "5r01")
  (parse-number "1.")
  (parse-number "01.0")
  (parse-number "1.0%")
  (parse-number "1.%d")
  (parse-number "-1%f")
  (parse-number "2f")
  (parse-number "2%")
  (parse-number "0xFFffffff")
  (parse-number "0f")
  (parse-number "4d")
  
  )

(defn parsed-number-value [{:keys [kind neg number] :as m}]
  (condp = kind
    :int (let [{:keys [radix unsigned?]} m
               number (if neg (str "-" number) number)
               n (condp = (:type m)
                   :none (if unsigned?
                          (Integer/parseUnsignedInt number radix)
                          (Integer/parseInt number radix))
                   :long (if unsigned?
                           (Long/parseUnsignedLong number radix)
                           (Long/parseLong number radix))
                   :bigint (BigInteger. number radix)
                   :int (if unsigned?
                          (Integer/parseUnsignedInt number radix)
                          (Integer/parseInt number radix))
                   :byte (Byte/parseByte number radix))]
           n)
    :float (let [{:keys [percent?]} m
                 n (cond-> (BigDecimal. number) neg .negate)
                 n (if percent? (.divide n 100M) n)
                 n (condp = (:type m :double)
                     :none (.doubleValue n)
                     :double (.doubleValue n)
                     :bigdec n
                     :float (.floatValue n))]
             n)
    (throw (RuntimeException. "Could not decode parsed number"))))

(defprotocol PCollBuilder
  (-addEnd [_ x])
  (-toColl [_]))

(deftype CljListBuidler [^java.util.ArrayList alist]
  PCollBuilder
  (-addEnd [_ x] (.add alist x))
  (-toColl [_] (list* alist)))

(deftype CljVectorBuidler [^:unsynchronized-mutable tcoll]
  PCollBuilder
  (-addEnd [_ x] (set! tcoll (conj! tcoll x)))
  (-toColl [_] (persistent! tcoll)))

(deftype CljSetBuidler [^:unsynchronized-mutable tcoll]
  PCollBuilder
  (-addEnd [_ x]
    ; (when (contains? tcoll x) ...)
    (set! tcoll (conj! tcoll x)))
  (-toColl [_] (persistent! tcoll)))

(deftype CljMapBuidler [^:unsynchronized-mutable tcoll
                        ^:unsynchronized-mutable k
                        ^:unsynchronized-mutable ^Boolean hanging?]
  PCollBuilder
  (-addEnd [_ x]
    (if hanging?
      (do
        (when (contains? tcoll k)
          (throw (RuntimeException. (str "Duplicate key: " k))))
        (set! tcoll (assoc! tcoll k x))
        (set! hanging? false))
      (do (set! k x)
        (set! hanging? true))))
  (-toColl [_]
    (when hanging?
      (throw (RuntimeException. "Odd number of forms in map")))
    (persistent! tcoll)))

(defn clj-list-builder []
  (->CljListBuidler (java.util.ArrayList.)))
(defn clj-vector-builder []
  (->CljVectorBuidler (transient [])))
(defn clj-map-builder []
  (->CljMapBuidler (transient {}) nil false))
(defn clj-set-builder []
  (->CljSetBuidler (transient #{})))

(defn interpret-char-token [token]
  (cond
    (.equals token "newline") \newline
    (.equals token "space") \space
    (.equals token "tab") \tab
    (.equals token "backspace") \backspace
    (.equals token "formfeed") \formfeed
    (.equals token "return") \return
    (.startsWith "u")
    (throw (UnsupportedOperationException.
             "not implemented unicode character literal"))
    :else
    (throw (RuntimeException. (str "Invalid char: " token)))))

(deftype CljCollVisitor [parent cb]
  PFormVisitor
  (-visitNumber [_ numstr]
    (let [num (parse-number numstr)]
      (if (nil? num)
        (throw (RuntimeException. (str "Invalid number: " numstr)))
        (-addEnd cb (parsed-number-value num)))))
  (-visitKeyword [_ kwstr]
    (-addEnd cb (keyword (subs kwstr 1))))
  (-visitSymbol [_ symstr]
    (-addEnd cb (symbol symstr)))
  (-visitChar [_ token]
    (-addEnd cb (interpret-char-token token)))
  (-visitCharLiteral [_ c]
    (-addEnd cb (char c)))
  (-visitString [_ s]
    (-addEnd cb s))
  (-visitList [self] 
    (CljCollVisitor. self (clj-list-builder)))
  (-visitVector [self]
    (CljCollVisitor. self (clj-vector-builder)))
  (-visitMap [self]
    (CljCollVisitor. self (clj-map-builder)))
  (-visitSet [self]
    (CljCollVisitor. self (clj-set-builder)))
  (-visitDiscard [_] (reader/noop-visitor))
  (-visitEnd [_]
    (when parent
      (-addEnd (.-cb ^CljCollVisitor parent) (-toColl cb))))
  PCollBuilder
  (-toColl [_] (-toColl cb)))

(defn clj-rdrvisitor []
  (->CljCollVisitor nil (clj-vector-builder)))

(comment
  
  (let [v (clj-rdrvisitor)]
    (reader/read-all-forms v
      (reader/file-reader-default "src2/okcolour.clj"))
    (clojure.pprint/pprint (-toColl v)))

  )














