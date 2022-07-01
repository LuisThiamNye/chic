(ns chic.clipboard
  (:refer-clojure :exclude [get set])
  (:import
   (io.github.humbleui.jwm Clipboard ClipboardFormat ClipboardEntry)))

(def kw->format
  {:text:plain ClipboardFormat/TEXT})

(defn get [fmt]
  (let [a ^"[Lio.github.humbleui.jwm.ClipboardFormat;" (make-array ClipboardFormat 1)]
    (aset a 0 ^ClipboardFormat (kw->format fmt))
    (some->
     (Clipboard/get a)
     (cond->
       (contains? #{:text:plain :text:rtf :text:html :text:url} fmt)
       (.getString)))))

(defn set [_])

(defn set-text [text]
  (let [a ^"[Lio.github.humbleui.jwm.ClipboardFormat;" (make-array ClipboardEntry 1)]
    (aset a 0 ^ClipboardEntry (ClipboardEntry/makeString ClipboardFormat/TEXT text))
    (Clipboard/set a)))

(defn clear []
  (Clipboard/clear))
