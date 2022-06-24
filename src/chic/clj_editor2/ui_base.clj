(ns chic.clj-editor2.ui-base
  (:require
   [chic.ui :as cui]
   [chic.ui.ui2 :as cui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser]))

(def sample-ast (parser/read-fresh (java.io.StringReader. (slurp "src/chic/ui.clj"))))

(defn ui-coll-container [{}]
  (cui2/layout
   {:init {}}
   (ui/clip-rrect
    5
    (ui/fill (huipaint/fill 0x20000000)
             (ui/gap 100 100)))))

(def *layout (atom {1 {}}))

(defn sample-view []
  (cuilay/halign
   0 (cuilay/valign
      0 (cuilay/padding
         8
         (let [build (fn [_ctx item]
                       (cui2/layout
                        {:init {}
                         :then (fn [])}
                        (ui-coll-container {})))]
           (cui2/inf-column
           {:init
            (fn [_ctx])
            :prev
            (fn [ctx current]
              (build ctx 1))
            :next
            (fn [ctx current]
              (build ctx 1))}))))))

(comment
  (chic.windows/remount-all-windows)
  (::ast/root-node (::ast/nodes sample-ast))

  #!
  )
