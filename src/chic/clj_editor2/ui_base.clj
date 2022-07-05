(ns chic.clj-editor2.ui-base
  (:require
   [chic.style :as style]
   [chic.ui.font :as uifont]
   [chic.clj-editor2.file-tree :as file-tree]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.window :as huiwin]
   [chic.paint :as cpaint]
   [chic.ui.layout :as cuilay]
   [chic.windows :as windows]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.github.humbleui.jwm Window)
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

;; (def *layout (atom {1 {}}))

(defn build-menu-window-root [{}]
  (ui/dynamic
    ctx [{:keys [scale]} ctx]
    (ui/with-context
      {:font-ui (Font. style/face-code-default (float (* scale 12)))
       :fill-text (huipaint/fill 0xFF000000)
       :font-code (Font. style/face-code-default (float (* 12 scale)))}
      (ui2/v1-root
       {}
       (ui2/stack
        (file-tree/a-view))))))

(defn make-bwr-window [{:keys [window]}]
  {:pre [(map? window)]}
  (let [screen (.getScreen ^Window (:window-obj window))
        scale (.getScale screen)
        ;; area (.getWorkArea screen)
        width (* 800 scale)
        height (* 500 scale)
        window-rect (huiwin/window-rect (:window-obj window))
        ;; x (+ (:x window-rect) (:x mouse-pos))
        ;; y (+ (:y window-rect) (:y mouse-pos))
        w (windows/make2
           {:id (random-uuid)
            :build-app-root (fn [] (cui/dyncomp (build-menu-window-root {})))
            :on-close (fn [])})
        wo (:window-obj w)]
    (doto wo
      (huiwin/set-title (str "Browser"))
      (huiwin/set-window-size width height)
      #_(huiwin/set-window-position x y))
    ;; (huiwin/set-visible wo true)
    (windows/set-visible w true)
    (.focus wo)))

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
      (ui2/attach-interactor-manager
       {}
       (ui2/column
        ;; (ui2/fill-rect )
        (ui2/attach-interactor
         {:cursor-style :pointing-hand
          :on-mouse-down
          (fn [ctx rect evt]
            (make-bwr-window {:window (:chic/current-window ctx)}))}
         (ui2/text-string "Open tree" font))
        (ui2/padded
         8
         (ui2/clip-rect
          (ui2/inf-column
           {:next (fn build-next [_]
                    ui-item)})))))))))

(comment
  (chic.windows/remount-all-windows)
  (::ast/root-node (::ast/nodes sample-ast))

  #!
  )
