(ns jl.test.reader.parser
  (:require
    [jl.kickstart :as kickstart]
    [jl.test.util :as test.util :refer [with-test-deps eval-str]]
    [clojure.test :refer [deftest is testing] :as test]))

(def numparser-clsname "sq.compiler.parser.NumberParser")

(defn parse-number [numstr]
  #_(eval-str (str "(jc " numparser-clsname " parse-number " numstr ")"))
  (kickstart/rcall (kickstart/cof numparser-clsname)
    'parse-number numstr))

(defn test-number [num exp]
  (let [r (parse-number num)]
    (assert (= r (:number exp)) r)))

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
  {:number 1.})
(test-number "1.0"
  {:number 1.})
(test-number "1%"
  {:number 0.01})
(test-number "1.0%"
  {:number 0.01})
(test-number "1.%d"
  {:number 0.01 :type :double})
(test-number "-1%f"
  {:number (float 0.01) :sign :neg :type :float})
(test-number "2f"
  {:number (float 2) :type :float})
(test-number "2%"
  {:number 0.02})
(test-number "0"
  {:number 0})
(test-number "0f"
  {:number (float 0) :type :float})