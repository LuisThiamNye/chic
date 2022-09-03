(ns jl.reader
  (:require
    [tech.v3.datatype.char-input :as dtype.char-input] ;; TODO use charred
    [chic.util :refer [<- deftype+]])
  (:import
    (tech.v3.datatype CharReader)))

(defprotocol PReader
  (-read [_])
  (-unread [_])
  (-eof? [_]))

(defprotocol PTextPosition
  (-getLine [_])
  (-getCol [_]))

(defn anom! [rdr category msg]
  (throw (ex-info msg {:line (-getLine rdr)
                       :col (-getCol rdr)
                       :category category})))

(definline str-starts-with-char? [s c]
  `(let [~'s ~(vary-meta s assoc :tag `String)
         ~'c ~c]
     (if (< 0 (.length ~'s))
       (= (.charAt ~'s 0) ~'c)
       false)))

(defn whitespace? [^long cn]
  (or (Character/isWhitespace cn) (clojure.lang.Util/identical cn 44)))

; (defprotocol PReaderVisitor
  ; (-visitEof [_]))

(defprotocol PFormVisitor
  (-visitNumber [_ numstr])
  (-visitKeyword [_ kwstr])
  (-visitSymbol [_ symstr])
  (-visitChar [_ token])
  (-visitCharLiteral [_ c])
  (-visitString [_ s])
  (-visitList [_])
  (-visitVector [_])
  (-visitMap [_])
  (-visitSet [_])
  ; (-visitUnmatchedDelim [_ delim])
  (-visitDiscard [_])
  (-visitMeta [_]) ;; -> [meta-vtor value-vtor]
  (-visitEnd [_]))

(defn noop-visitor
  ([] (noop-visitor nil))
  ([dv]
   (reify PFormVisitor
     (-visitNumber [_ _])
     (-visitKeyword [_ _])
     (-visitSymbol [_ _])
     (-visitChar [_ _])
     (-visitCharLiteral [_ _])
     (-visitString [_ _])
     (-visitList [self] (noop-visitor self))
     (-visitVector [self] (noop-visitor self))
     (-visitMap [self] (noop-visitor self))
     (-visitSet [self] (noop-visitor self))
     (-visitDiscard [self] dv)
     (-visitMeta [self] [(noop-visitor self) (noop-visitor self)])
     (-visitEnd [_]))))

"Model:
Literals:
- symbol
- keyword
- character
- number
- list
- vector
- map
- set

Number types:
- byte b
- short i16
- int i
- long L
- float f
- double d
- BigInt N
- BigDecimal M
- Ratio
- Complex _+_ _arg_
Number repr:
- binary 0b
- hex 0x
- radix _r
- octal 0
- percent _%
"

(declare macros disp-macros)

(defn get-macro [c]
  (when (< c (alength macros))
    (aget macros c)))

(defn get-dispatch-macro [c]
  (aget disp-macros c))

(defn macro? [c]
  (and (< c (alength macros)) (not (nil? (aget macros c)))))

(defn terminating-macro? [c]
  (case (char c) (\# \' \%) false (macro? c)))

(defn read-str-escape [rdr]
  (let [c (-read rdr)]
    (when (= -1 c)
      (throw (RuntimeException. "EOF reading string")))
    (case (char c)
      \t \tab
      \r \return
      \n \newline
      \\ \\
      \" \"
      \b \backspace
      \f \formfeed
      \u (let [c (-read rdr)]
           (if (= -1 (Character/digit c 16))
             (throw (RuntimeException. (str "Invalid unicode escape: " (char c))))
             (throw (UnsupportedOperationException. "Unicode not implemented"))))
      (throw (RuntimeException. (str "Unsupported escape sequence: " (char c)))))))

(defn read-nws [rdr]
  (loop []
    (let [c (-read rdr)]
      (if (whitespace? c)
        (recur)
        c))))

(defn read-token
  "Always returns valid token string"
  [rdr c1]
  (let [sb (doto (StringBuilder.) (.append (char c1)))]
    (loop []
      (let [c (-read rdr)]
        (when-not (== -1 c)
          (if (or (whitespace? c) (terminating-macro? c))
            (-unread rdr)
            (do (.append sb (char c))
                (recur))))))
    (.toString sb)))

(declare read-next-form read-next-form*)

(defn mread-string [vtor rdr _doublequote]
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (-read rdr)]
        (when-not (= c (int \"))
          (when (= -1 c)
            (throw (RuntimeException. "EOF reading string")))
          (if (= (int \\) c)
            (some->> (read-str-escape rdr)
              (.append sb))
            (.append sb (char c)))
          (recur))))
    (-visitString vtor (.toString sb))))

(defn mread-comment [_vtor rdr _semicolon]
  (loop []
    (let [c (-read rdr)]
      (when-not (or (= -1 c) (= (int \newline) c))
        (recur)))))

(defn mread-quote [vtor rdr _quote]
  (let [symname "_§!S_quote"
        lv (-visitList vtor)]
    (-visitSymbol lv symname)
    (read-next-form lv rdr)
    (-visitEnd lv)))

(defn mread-char [vtor rdr _backslash]
  (let [c (-read rdr)
        _ (when (= -1 c) (throw (RuntimeException. "EOF reading char")))
        token (read-token rdr c)]
    (if (= 1 (count token))
      (-visitCharLiteral vtor (.charAt token 0))
      (-visitChar vtor token))))

(defn mread-meta [vtor rdr _caret]
  (let [[mv vv] (-visitMeta vtor)]
    (read-next-form mv rdr)
    (read-next-form vv rdr)
    (-visitEnd vv))
  #_(let [lv (-visitList vtor)]
    (-visitSymbol lv "_§!S_apply-meta")
    (read-next-form lv rdr)
    (read-next-form lv rdr)
    (-visitEnd lv)))

(defn read-delimited-list [vtor rdr endc]
  (loop []
    (let [c (read-nws rdr)]
      (cond
        (= c endc) nil
        (< c 0) (throw (RuntimeException. (str "EOF reading list: " (char endc))))
        :else
        (do (read-next-form* vtor rdr c)
          (recur)))))
  (-visitEnd vtor))

(defn mread-list [vtor rdr _openbracket]
  (read-delimited-list (-visitList vtor) rdr (int \) )))

(defn mread-vector [vtor rdr _openbracket]
  (read-delimited-list (-visitVector vtor) rdr (int \] )))

(defn mread-map [vtor rdr _openbracket]
  (read-delimited-list (-visitMap vtor) rdr (int \} )))
  
(defn mread-set [vtor rdr _openbracket]
  (read-delimited-list (-visitSet vtor) rdr (int \} )))

(defn mread-unmatched-delim [_vtor rdr delim]
  (anom! rdr :incorrect (str "Unmatched delimiter: " (char delim)))
  #_(-visitUnmatchedDelim vtor delim))

(defn mread-dispatch [vtor rdr _hash]
  (let [c (-read rdr)
        _ (when (= -1 c) (throw (RuntimeException. "EOF reading dispatch")))
        f (get-dispatch-macro c)]
    (when (nil? f)
      (throw (RuntimeException. (str "No dispatch macro found for: " (char c)))))
    (f vtor rdr c)))

(defn mread-discard [vtor rdr _underscore]
  (let [dv (-visitDiscard vtor)]
    (if dv
      (read-next-form dv rdr)
      (do (read-next-form vtor rdr)
        (read-next-form vtor rdr)))))

(def ^"[Lclojure.lang.IFn;" macros
  (doto ^"[Lclojure.lang.IFn;" (make-array clojure.lang.IFn 256)
    (aset \" #'mread-string)
    (aset \; #'mread-comment)
    (aset \' #'mread-quote)
    ; (aset \@ #'mread-deref!)
    (aset \^ #'mread-meta)
    ; (aset \` mread-syntax-quote!)
    ; (aset \~ mread-unquote!)
    (aset \( #'mread-list)
    (aset \) #'mread-unmatched-delim)
    (aset \[ #'mread-vector)
    (aset \] #'mread-unmatched-delim)
    (aset \{ #'mread-map)
    (aset \} #'mread-unmatched-delim)
    (aset \\ #'mread-char)
    ; (aset \% #'mread-arg!)
    (aset \# #'mread-dispatch)
    ))

(def ^"[Lclojure.lang.IFn;" disp-macros
  (doto ^"[Lclojure.lang.IFn;" (make-array clojure.lang.IFn 256)
    ; (aset \# mread-symbolic-val)
    ; (aset \' mread-var)
    ; (aset \" mread-regex)
    ; (aset \( mread-fn)
    (aset \{ #'mread-set)
    ; (aset \= mread-eval)
    ; (aset \! mread-comment)
    ; (aset \< mread-unreadable)
    (aset \_ #'mread-discard)
    ; (aset \? mread-conditional)
    ; (aset \: mread-namespaced-map)
    ))

(defn read-number
  "Always returns valid number string"
  [rdr c1]
  (let [sb (doto (StringBuilder.) 
             (.append (char c1)))]
    (loop []
      (let [c (-read rdr)]
        (when (<= 0 c)
          (if (or (macro? c) (whitespace? c))
            (-unread rdr)
            (do (.append sb (char c))
                (recur))))))
    (.toString sb)))

(defn read-next-form* [vtor rdr c]
  (<- (if (Character/isDigit c)
        (-visitNumber vtor (read-number rdr c)))
          
    (let [macro-fn (get-macro c)])
    (if macro-fn
      (macro-fn vtor rdr c))
          
    (or (when (or (= (int \+) c) (= (int \-) c))
          (let [c2 (-read rdr)
                num? (Character/isDigit c2)]
            (if num?
              (do (-unread rdr)
                (-visitNumber vtor (read-number rdr c)))
              (-unread rdr))
            num?)))
          
    (let [token (read-token rdr c)]
      (if (str-starts-with-char? token \:)
        (-visitKeyword vtor token)
        (-visitSymbol vtor token)))))

(defn read-next-form [vtor rdr]
  (when-not (-eof? rdr)
    (let [c (read-nws rdr)]
      (when (<= 0 c)
        (read-next-form* vtor rdr c)
        true))))

(defn read-all-forms [v rdr]
  (while (read-next-form v rdr)))

(deftype+ TrackedCharReader
  [^CharReader rdr
   ^:mut ^int line ^:mut ^int col ^:mut ^int prev-col]
  PReader
  (-read [_]
    (let [r (.read rdr)]
      (set! prev-col col)
      (cond
        (= r 10)
        (do (set! line (unchecked-inc-int line))
          (set! col (unchecked-int 1)))
        (not (or (= r 13) (= r -1)))
        (do (set! col (unchecked-inc-int col))))
      r))
  (-unread [_]
    (cond
      (= prev-col col) nil
      (= 1 col) (set! line (unchecked-dec-int line))
      :else (set! col (unchecked-dec-int col)))
    (.unread rdr))
  (-eof? [_] (.eof rdr))
  PTextPosition
  (-getLine [_] line)
  (-getCol [_] col))

(defn tracked-charreader [^CharReader rdr]
  (->TrackedCharReader rdr 1 1 1))

(defn file-reader-default 
  "Acceps File, FileDescriptor or String"
  [f]
  (tracked-charreader
    (dtype.char-input/reader->char-reader
      (java.io.InputStreamReader.
        (java.io.FileInputStream. f))
      {:bufsize 1024})))

(defn str-reader-default [s]
  (tracked-charreader
    (dtype.char-input/reader->char-reader
      (java.io.StringReader. s)
      {:bufsize 1024})))



