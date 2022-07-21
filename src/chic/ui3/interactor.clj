(ns chic.ui3.interactor
  (:require
   [potemkin :refer [doit]]
   [chic.ui2.event :as ievt]
   [chic.debug :as debug]
   [chic.util :as util :refer [deftype+ setf!]])
  (:import
   (io.github.humbleui.types Rect)))

(deftype+ StdIntrMgr [^:mut ^java.util.List intrs]
  :keys true
  :settable true)

(defn new-manager []
  (->StdIntrMgr (java.util.ArrayList.)))

(defn point-in-rect?
  ([rect point] (point-in-rect? rect (:x point) (:y point)))
  ([^Rect rect x y]
   (and (<= (.getLeft rect) x)
        (<= (.getTop rect) y)
        (< x (.getRight rect))
        (< y (.getBottom rect)))))

(defn -handle-mouse-scroll [mgr ctx evt]
  (let [it (util/to-iter (:intrs mgr))]
    (loop []
      (when-some [intr (util/iter-next it)]
        (let [rect (:rect intr)
              handled? (and (point-in-rect? rect (:chic.ui.ui2/mouse-pos ctx))
                            (when-some [f (.get ^java.util.Map (:handlers intr) :on-scroll)]
                              (f intr ctx evt) true))]
          (when-not handled? (recur)))))))

(defn -handle-mousedown [mgr ctx evt])
(defn -handle-mouseup [mgr ctx evt])
(defn -handle-mouse-move [mgr ctx evt])
(defn -handle-keydown [mgr ctx evt])
(defn -handle-keyup [mgr ctx evt])
(defn -handle-textinput [mgr ctx evt])

(defn -handle-jwm-event [mgr ctx evt]
  (ievt/case-event evt
    :mouse-move (-handle-mouse-move mgr ctx evt)
    :mouse-scroll (-handle-mouse-scroll mgr ctx evt)
    :mouse-button (if (ievt/mousebtn-down? evt)
                    (-handle-mousedown mgr ctx evt)
                    (-handle-mouseup mgr ctx evt))
    :key (if (ievt/key-down? evt)
           (-handle-keydown mgr ctx evt)
           (-handle-keyup mgr ctx evt))
    :text-input (-handle-textinput mgr ctx evt)))

(defn -unreg-interactor [mgr intr]
  (let [intrs ^java.util.List (:intrs mgr)
        idx (.indexOf intrs intr)
        _ (.remove intrs idx)]))

(deftype+ StdIntr [mgr ^:mut ^Rect rect
                   ^java.util.Map handlers]
  :keys true
  :settable true
  java.lang.AutoCloseable
  (close [self] (-unreg-interactor mgr self)))

(defn open-interactor [mgr opts]
  (let [intr (->StdIntr mgr (Rect. -1. -1. -1. -1.)
                        (java.util.HashMap.))]
    (.add ^java.util.List (:intrs mgr) intr)
    intr))

(defn refresh-intr [intr opts]
  (setf! intr :rect (:rect opts))
  (let [handlers ^java.util.Map (:handlers intr)]
    (when (contains? opts :on-scroll)
      (.put handlers :on-scroll (:on-scroll opts)))))
