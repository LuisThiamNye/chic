(ns chic.puget
  (:use
    [puget.printer :as puget :exclude [map->PrettyPrinter pretty-printer]])
  (:require
    [chic.util :as util]
    [fipp.visit :as fv]
    [arrangement.core :as order]
    [puget.color :as color]))

(util/inherit-vars
  puget/trim-coll?
  puget/order-collection
  puget/common-key-ns)


;; Changed so that seqs have align offset of 1
(defrecord PrettyPrinter
  [width
   print-meta
   sort-keys
   map-delimiter
   map-coll-separator
   namespace-maps
   seq-limit
   coll-limit
   print-color
   color-markup
   color-scheme
   print-handlers
   print-fallback]

  fv/IVisitor

  ;; Primitive Types

  (visit-nil
    [this]
    (color/document this :nil "nil"))


  (visit-boolean
    [this value]
    (color/document this :boolean (str value)))


  (visit-number
    [this value]
    (color/document this :number (pr-str value)))


  (visit-character
    [this value]
    (color/document this :character (pr-str value)))


  (visit-string
    [this value]
    (color/document this :string (pr-str value)))


  (visit-keyword
    [this value]
    (color/document this :keyword (str value)))


  (visit-symbol
    [this value]
    (color/document this :symbol (str value)))


  ;; Collection Types

  (visit-seq
    [this value]
    (if (seq value)
      (let [limit (or seq-limit coll-limit)

            [elements trimmed?]
            (if (pos-int? limit)
              (let [head (take (inc limit) value)]
                [(take limit head) (< limit (count head))])
              [value false])

            elements
            (if (symbol? (first elements))
              (cons (color/document this :function-symbol (str (first elements)))
                    (map (partial format-doc this) (rest elements)))
              (map (partial format-doc this) elements))

            elements
            (if trimmed?
              (concat elements [(color/document this :nil "...")])
              elements)]
        [:group
         (color/document this :delimiter "(")
         [:align 1 (interpose :line elements)]
         (color/document this :delimiter ")")])
      (color/document this :delimiter "()")))


  (visit-vector
    [this value]
    (if (seq value)
      (let [trimmed? (trim-coll? value coll-limit)
            elements (map (partial format-doc this)
                          (if trimmed?
                            (take coll-limit value)
                            value))
            elements (if trimmed?
                       (concat elements [(color/document this :nil "...")])
                       elements)]
        [:group
         (color/document this :delimiter "[")
         [:align (interpose :line elements)]
         (color/document this :delimiter "]")])
      (color/document this :delimiter "[]")))


  (visit-set
    [this value]
    (if (seq value)
      (let [trimmed? (trim-coll? value coll-limit)
            elements (map (partial format-doc this)
                          (if trimmed?
                            (take coll-limit value)
                            (order-collection sort-keys value (partial sort order/rank))))

            elements (if trimmed?
                       (concat elements [(color/document this :nil "...")])
                       elements)]
        [:group
         (color/document this :delimiter "#{")
         [:align (interpose :line elements)]
         (color/document this :delimiter "}")])
      (color/document this :delimiter "#{}")))


  (visit-map
    [this value]
    (if (seq value)
      (let [trimmed? (trim-coll? value coll-limit)

            elements
            (if trimmed?
              (take coll-limit value)
              value)

            [common-ns stripped]
            (when namespace-maps
              (common-key-ns value))

            elements
            (if trimmed?
              elements
              (order-collection
                sort-keys
                (or stripped elements)
                (partial sort-by first order/rank)))

            elements
            (map (fn render-kv
                   [[k v]]
                   [:span
                    (format-doc this k)
                    (if (coll? v)
                      map-coll-separator
                      " ")
                    (format-doc this v)])
                 elements)

            elements
            (if trimmed?
              (concat elements [(color/document this :nil "...")])
              elements)

            map-doc
            [:group
             (color/document this :delimiter "{")
             [:align (interpose [:span map-delimiter :line] elements)]
             (color/document this :delimiter "}")]]
        (if common-ns
          [:group (color/document this :tag (str "#:" common-ns)) :line map-doc]
          map-doc))
      (color/document this :delimiter "{}")))


  ;; Clojure Types

  (visit-meta
    [this metadata value]
    (if print-meta
      [:group
       [:span (color/document this :delimiter "^") (format-doc this metadata)]
       :line (format-doc* this value)]
      (format-doc* this value)))


  (visit-var
    [this value]
    [:span
     (color/document this :delimiter "#'")
     (color/document this :symbol (subs (str value) 2))])


  (visit-pattern
    [this value]
    [:span
     (color/document this :delimiter "#")
     (color/document this :string (str \" value \"))])


  (visit-record
    [this value]
    (fv/visit-tagged
      this
      (tagged-literal (symbol (.getName (class value)))
                      (into {} value))))


  ;; Special Types

  (visit-tagged
    [this value]
    (let [form (:form value)]
      [:group
       (color/document this :tag (str "#" (:tag value)))
       (if (coll? form) :line " ")
       (format-doc this form)]))


  (visit-unknown
    [this value]
    (case print-fallback
      :pretty
      (format-unknown this value)

      :print
      [:span (pr-str value)]

      :error
      (throw (IllegalArgumentException.
               (str "No defined representation for " (class value) ": "
                    (pr-str value))))

      (if (ifn? print-fallback)
        (print-fallback this value)
        (throw (IllegalStateException.
                 (str "Unsupported value for print-fallback: "
                      (pr-str print-fallback))))))))


(defn pretty-printer
  "Constructs a new printer from the given configuration."
  [opts]
  (->> [{:print-meta *print-meta*
         :print-handlers common-handlers}
        *options*
        opts]
       (reduce merge-options)
       (map->PrettyPrinter)))

(ns-unmap *ns* '->PrettyPrinter)
