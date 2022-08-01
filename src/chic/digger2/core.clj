(ns chic.digger2.core
  (:require
    [chic.types :as types]
    [chic.ui2.event :as ievt]
    [chic.ui.canvas :as cnv]
    [chic.ui3.interactor :as uii3]
    [chic.util :as util :refer [deftype+ <-]]
    [chic.windows :as windows]
    [chic.digger2.inspector :as inspector]
    [chic.ui.ui3 :as ui3])
  (:import
    (java.lang AutoCloseable)
    (io.github.humbleui.types Rect IRect)
    (chic.types IXyIM)
    (io.github.humbleui.jwm Window)))

(ui3/deffnletcmpt ui-root [intrmgr rect ^float scale]
  (let [cmpt-flow (ui3/new-cmpt inspector/ui-inspector-flow)]
    {:draw 
     (fn [cnv]
       (.clear cnv (unchecked-int 0xFFffffff))
       (ui3/draw-cmpt cmpt-flow cnv 
         {:rect rect :scale scale :intrmgr intrmgr}))}))

(declare new-window-box)
(deftype+ DiggerWindowV3 [^:mut ^Window jwm-window
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
  chic.windows.RenewableWindow
  (renew [self] 
    (new-window-box jwm-window)
    (.close self))
  java.lang.AutoCloseable
  (close [self]
    (do (windows/unreg-window! self)
      (when jwm-window
        (.close ^AutoCloseable root)
        (set! jwm-window nil)))))

(defn new-window-box [^Window jwin]
  (let [mouse-pos (types/->XyIMunsync -1 -1)
        ctx {:chic.ui/mouse-win-pos mouse-pos}]
    (doto (->DiggerWindowV3 jwin
              #_ctx ctx
              #_scale 1.
              #_rect (Rect. -1 -1 -1 -1)
              #_mouse-pos mouse-pos
              #_intrmgr (uii3/new-manager)
              #_chmask Integer/MAX_VALUE
              #_root (util/clearing-thread-locals
                       [windows/*root-ctx ctx]
                       (ui3/new-cmpt ui-root)))
      (windows/register-window!)
      (->> (windows/set-event-listener jwin)))))

(defn open-new-window [rect]
  (let []
    (util/with-keep-open 
      [jw (windows/make-jwm-window)
       w (new-window-box jw)]
      (util/setf! w :jwm-window jw)
      (doto jw
        (.setTitle "Digger")
        (windows/set-window-rect rect)
        (.setVisible true))
      (.focus jw))))

