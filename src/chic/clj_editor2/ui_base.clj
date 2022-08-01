(ns chic.clj-editor2.ui-base
  (:require
    [chic.style :as style]
    [chic.ui.font :as uifont]
    [chic.clj-editor2.file-tree :as file-tree]
    [chic.ui :as cui]
    [chic.debug :as debug]
    [chic.ui.ui2 :as ui2]
    [chic.ui.ui3 :as ui3]
    [chic.clj-editor.ast :as ast]
    [io.github.humbleui.paint :as huipaint]
    [chic.paint :as cpaint]
    [chic.util :as util :refer [deftype+ <-]]
    [chic.windows :as windows]
    [chic.ui2.event :as ievt]
    [chic.ui3.interactor :as uii3]
    [io.github.humbleui.ui :as ui]
    [chic.types :as types]
    [chic.clj-editor.parser :as parser])
  (:import
    (java.lang AutoCloseable)
    (chic.types IXyIM)
    (io.github.humbleui.jwm Window)
    (io.github.humbleui.skija Paint Font)
    (io.github.humbleui.types Rect Point IRect)))

;(def sample-ast (parser/read-fresh (java.io.StringReader. (slurp "src/chic/ui.clj"))))

(ui3/deffnletcmpt ui-root [intrmgr ^float scale rect]
  (let [dev-view (ui3/new-cmpt file-tree/ui-dev-view)]
    {:draw 
     (fn [cnv]
       (.clear cnv (unchecked-int 0xFFffffff))
       (ui3/draw-cmpt
         dev-view cnv
         {:rect rect
          :visible-rect rect
          :scale scale
          :intrmgr intrmgr}))}))

(deftype+ EditorWindowV3 [^:mut ^Window jwm-window
                          ^:mut ctx
                          ^:mut ^float scale
                          ^:mut ^Rect rect
                          ^IXyIM mouse-pos
                          intrmgr
                          ^:mut ^int chmask
                          root]
  :keys [jwm-window] 
  :settable [jwm-window]
  chic.windows.PaintAndEventHandler
  (paint [_ cnv]
    (util/clearing-thread-locals 
      [windows/*root-ctx ctx]
      (ui3/draw-cmpt-ext ^{:cmpt ui-root} root cnv chmask
        {:intrmgr intrmgr
         :rect rect
         :scale scale}))
    (set! chmask (unchecked-int 0)))
  (notifyEvent [self evt]
    (ievt/case-event evt
      :frame nil
      :window-close-req (.close jwm-window)
      :window-close (.close self)
      (do (ievt/case-event evt
            :window-resize
            (let [content-rect (.getContentRect jwm-window)]
              (set! rect (Rect. 0 0 (.getWidth content-rect) (.getHeight content-rect)))
              (set! chmask (unchecked-int (bit-set chmask 1))))
            :window-screen-change
            (do (set! scale (.getScale (.getScreen jwm-window)))
              (set! chmask (unchecked-int (bit-set chmask 2))))
            :mouse-move
            (.resetXy mouse-pos (ievt/mouse-x evt) (ievt/mouse-y evt)))
        (uii3/-handle-jwm-event intrmgr ctx evt)
        (.requestFrame jwm-window))))
  java.lang.AutoCloseable
  (close [self]
    (do (windows/unreg-window! self)
      (.close ^AutoCloseable root))))

(defn new-window-box []
  (let [mouse-pos (types/->XyIMunsync -1 -1)
        ctx {:chic.ui/mouse-win-pos mouse-pos}]
    (doto (->EditorWindowV3 nil 
              #_ctx ctx
              #_scale 1.
              #_rect (Rect. -1 -1 -1 -1)
              #_mouse-pos mouse-pos
              #_intrmgr (uii3/new-manager)
              #_chmask Integer/MAX_VALUE
              #_root (util/clearing-thread-locals
                       [windows/*root-ctx ctx]
                       (ui3/new-cmpt ui-root)))
      (windows/register-window!))))

(defn open-new-bwr-window [rect]
  (util/with-keep-open 
      [w (new-window-box)
       jw (windows/make-jwm-window w)]
      (util/setf! w :jwm-window jw)
      (doto jw
        (.setTitle "Browser")
        (windows/set-window-rect rect)
        (.setVisible true))
      (.focus jw)))

(defn derive-new-window-position [^Window window]
  (let [screen (.getScreen window)
        scale (.getScale screen)
        area (.getWorkArea screen)
        width (* 800 scale)
        height (* 500 scale)]
    (IRect/makeXYWH (:x area) (:y area) width height)))

(defn make-bwr-window [{:keys [window]}]
  {:pre [(map? window)]}
  (open-new-bwr-window (derive-new-window-position (:window-obj window))))

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
          (fn [ctx _rect _evt]
            (make-bwr-window {:window (:chic/current-window ctx)}))}
         (ui2/text-string "Open tree" font))
         (ui2/attach-interactor
           {:cursor-style :pointing-hand
            :on-mouse-down
            (fn [ctx _rect _evt]
              ((requiring-resolve `chic.digger2.core/open-new-window)
               (derive-new-window-position 
                 (:window-obj (:chic/current-window ctx)))))}
           (ui2/text-string "Open digger" font))
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
