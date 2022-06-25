(ns chic.clj-editor2.ui-base
  (:require
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.github.humbleui.skija Paint Font)
   (io.github.humbleui.types Rect Point)))

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
  (let [font (Font. style/face-code-default (float 24))
        text-line (uifont/shape-line-default font "This is a text line yum.")
        ui-item (ui2/sized-with
                 (fn [_]
                   (Point. (+ 16 (.getWidth text-line))
                           (+ 16 (Math/ceil (.getCapHeight text-line)))))
                 (ui2/stack
                  (ui2/fill-rrect 10 (huipaint/fill (cpaint/okhsv 0.6 0.12 0.96)))
                  (ui2/padded
                   8 (ui2/textline text-line))))]
    (ui/clip
     (ui2/v1-root
      {}
      (ui2/stack
       ;; (ui2/fill-rect )
       (ui2/padded
        8
        (ui2/clip-rect
         (ui2/inf-column
          {:next (fn build-next [_]
                   ui-item)}))))))))

(comment
  (chic.windows/remount-all-windows)
  (::ast/root-node (::ast/nodes sample-ast))

  #!
  )
