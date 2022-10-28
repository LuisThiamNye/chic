(ns jl.rv-data
  (:require
    [chic.util :refer [<-]]
    [clojure.walk :as walk]
    [lambdaisland.regal :as regal]
    [jl.reader :as reader :refer [PFormVisitor]])
  (:import
    (org.petitparser.parser.primitive CharacterParser)
    (org.petitparser.parser Parser)
    (java.util.function Function)))

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

  
(defmacro parsers [& args]
  `(into-array Parser [~@args]))
  
(def pp-sign
  (.map (.optional (.or (CharacterParser/of \-)
                     (parsers (CharacterParser/of \+))))
    (reify Function
      (apply [_ r]
        (if (nil? r)
          :pos-implicit
          (if (= \- r)
            :neg :pos))))))
  
(defn bigint-fn-with-radix [rad]
  (reify Function
    (apply [_ r]
      [(BigInteger. ^String r ^int rad) rad])))
  
(def pp-radix-num
  (.or
    (-> (.or
          (.seq (CharacterParser/of \x)
            (parsers
              (.map (.flatten (.plus (CharacterParser/pattern "0-9a-fA-F")))
                (bigint-fn-with-radix 16))))
          (parsers
            (.seq (CharacterParser/of \b)
              (parsers
                (.map (.flatten (.plus (CharacterParser/anyOf "01")))
                  (bigint-fn-with-radix 2))))))
      (.pick 1))
    (parsers
      (.map (.flatten (.plus (CharacterParser/range \0 \7)))
        (bigint-fn-with-radix 8)))))
  
(def pp-int-type
  (.map (CharacterParser/anyOf "biLN")
    (reify Function
      (apply [_ r]
        (case (char r)
          \b :byte
          \i :int
          \L :long
          \N :bigint)))))
  
(def pp-dec-type
  (.map (CharacterParser/anyOf "fdM")
    (reify Function
      (apply [_ r]
        (case (char r)
          \f :float
          \d :double
          \M :bigdec)))))
  
(def pp-arb-radix
  (->
    (.seq (.flatten
            (.seq (CharacterParser/range \1 \9)
              (parsers (.star (CharacterParser/digit)))))
      (parsers
        (CharacterParser/of \r)
        (.flatten (.plus (CharacterParser/pattern "0-9a-zA-Z")))))
    (.map
      (reify Function
        (apply [_ r]
          (let [radix (Integer/parseInt (.get r 0))]
            [(BigInteger. (.get r 2) radix) radix]))))))
  
(def pp-percent
  (.optional (.map (CharacterParser/of \%)
               (reify Function (apply [_ _] true)))
    false))
  
(def pp-int
  (.end
    (.seq
      (.or
        (->
          (.seq (CharacterParser/of \0)
            (parsers pp-radix-num))
          (.pick 1))
        (parsers
          pp-arb-radix
          (.map (.flatten (.plus (CharacterParser/digit)))
            (bigint-fn-with-radix 10))))
      (parsers
        (.optional pp-int-type :none)))))
  
(def pp-decimal
  (->
    (.seq
      (.flatten
        (.or (.seq (CharacterParser/range \1 \9)
               (parsers (.star (CharacterParser/digit))))
          (parsers (CharacterParser/of \0))))
      (parsers
          
        (.or
          (.map
            (.seq
              (CharacterParser/of \.)
              (parsers
                (.star (CharacterParser/digit))
                pp-percent
                (.optional pp-dec-type :none)))
            (reify Function
              (apply [_ r]
                [(.get r 1) (.get r 2) (.get r 3)])))
          (parsers
            (.map
              (.or (.seq (CharacterParser/of \%)
                     (parsers
                       (.optional pp-dec-type :none)))
                (parsers
                  pp-dec-type))
              (reify Function
                (apply [_ r]
                  (let [perc? (not (keyword? r))]
                    ["" perc? (if (not perc?) r (.get r 1))]))))))))
    (.map (reify Function
            (apply [_ r]
              (let [sb (doto (StringBuilder.)
                         (.append (.get r 0)))
                    [fracpart percent? typ] (.get r 1)
                    _ (.append sb \.)
                    _ (doseq [c fracpart]
                        (.append sb c))
                    bd (BigDecimal. (.toString sb))
                    bd (if percent?
                         (.divide bd 100M)
                         bd)]
                [bd typ]))))))
  
(def pp
  (.end
    (.seq
      pp-sign
      (parsers
        (.or
          pp-int
          (parsers
            (.map pp-decimal
              (reify Function
                (apply [_ r]
                  [[(nth r 0) 10] (nth r 1)])))))))))

(comment
  017M
    
  (defn num-parse [n]
    (let [r (.get (.parse pp n))
          _ (prn r)
          r-int (.get r 1)
          sign (.get r 0)
          radix (nth (nth r-int 0) 1)]
      {:number (nth (nth r-int 0) 0)
       :sign sign
       :radix radix
       :unsigned? (and (not (= 10 radix))
                    (= sign :pos-implicit))
       :type (nth r-int 1)}))
  
  (defn test-number [n exp]
    (let [res (num-parse n)]
      (prn res)
      (= (merge
           {:sign :pos-implicit
            :radix 10
            :unsigned? false
            :type :none}
           exp)
        res)))
  
  (test-number "12"
    {:number 12N})
  (test-number "+12"
    {:number 12 :sign :pos})
  (test-number "012"
    {:number 012 :radix 8 :unsigned? true})
  (test-number "0x12"
    {:number 0x12 :radix 16 :unsigned? true})
  (test-number "0x0F"
    {:number 0x0F :radix 16 :unsigned? true})
  (test-number "0x0d"
    {:number 0x0d :radix 16 :unsigned? true})
  (test-number "0b10"
    {:number 2r10 :radix 2 :unsigned? true})
  (test-number "0b10N"
    {:number 2r10 :radix 2 :unsigned? true :type :bigint})
  (test-number "5r01"
    {:number 5r01 :radix 5 :unsigned? true})
  (test-number "30r0N"
    {:number 30r0N :radix 30 :unsigned? true})
  (test-number "1."
    {:number 1M})
  (test-number "1.0"
    {:number 1M})
  (test-number "1%"
    {:number 0.01M})
  (test-number "1.0%"
    {:number 0.01M})
  (test-number "1.%d"
    {:number 0.01M :type :double})
  (test-number "-1%f"
    {:number 0.01M :sign :neg :type :float})
  (test-number "2f"
    {:number 2M :type :float})
  (test-number "2%"
    {:number 0.02M})
  (test-number "0"
    {:number 0})
  (test-number "0f"
    {:number 0M :type :float})
  
  )

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














