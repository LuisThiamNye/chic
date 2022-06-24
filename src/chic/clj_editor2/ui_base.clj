(ns chic.clj-editor2.ui-base
  (:require
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.github.humbleui.types Rect)))

(def sample-ast (parser/read-fresh (java.io.StringReader. (slurp "src/chic/ui.clj"))))

#_(defn ui-coll-container [{}]
  (cui2/layout
   {:init {}}
   (ui/clip-rrect
    5
    (ui/fill (huipaint/fill 0x20000000)
             (ui/gap 100 100)))))

(def *layout (atom {1 {}}))

(defn sample-view []
  #_(cuilay/halign
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
              (build ctx 1))})))))
  (ui2/v1-root
   {}
   (ui2/stack
    ;; (ui2/fill-rect )
    (ui2/adapt-rect
     (fn [_ ^Rect rect] (.inflate rect -8))
     (ui2/stack
      (ui2/margin
       8 (ui2/ph-textline
          "This is a text line."
          {:background
           [(ui2/fill-rrect 10 (huipaint/fill (cpaint/okhsv 0.6 0.12 0.96)))]})))))))

(comment
  (chic.windows/remount-all-windows)
  (::ast/root-node (::ast/nodes sample-ast))

  #!
  )
