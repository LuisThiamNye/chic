(ns chic.stdwin
  (:require
    [chic.types :as types]
    [chic.ui2.event :as ievt]
    [chic.ui3.interactor :as uii3]
    [chic.util :as util :refer [deftype+ <-]]
    [chic.windows :as windows]
    [chic.ui.ui3 :as ui3])
  (:import
    (java.lang AutoCloseable)
    (chic.types IXyIM)))

(ui3/defdrawinterface StdUiRoot)

(deftype+ StdWindowV3 [^:mut ^Window jwm-window
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
    (doto (->StdWindowV3 nil 
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