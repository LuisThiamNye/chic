(ns chic.ui2.event
  (:require
    [chic.util :as util])
  (:import
    (io.github.humbleui.jwm.skija EventFrameSkija)
    (io.github.humbleui.jwm MouseButton Event EventFrame EventKey 
      EventMouseButton EventMouseMove EventMouseScroll EventTextInput EventTextInputMarked
      EventWindowClose EventWindowCloseRequest EventWindowFocusIn
      EventWindowFocusOut EventWindowMaximize EventWindowMinimize
      EventWindowMove EventWindowResize EventWindowRestore
      EventWindowScreenChange)) 
  (:require
    [io.github.humbleui.ui :as ui]))

(def kw->mouse-button
  {:primary MouseButton/PRIMARY
   :secondary MouseButton/SECONDARY
   :middle MouseButton/MIDDLE
   :back MouseButton/BACK
   :FORWARD MouseButton/FORWARD})

(defmacro case-mousebtn [e & clauses]
  (let [kw->mouse-button-ord #(.ordinal ^MouseButton (kw->mouse-button %))
        xf-test (fn [expr]
                  (cond
                    (keyword? expr)
                    (kw->mouse-button-ord expr)
                    (seq? expr)
                    (map kw->mouse-button-ord expr)))
        pairs (map (fn [[test then]]
                     [(xf-test test) then])
                   (partition 2 clauses))
        fallback (when (odd? (count clauses))
                   (last clauses))
        e (vary-meta e assoc :tag (symbol (.getName EventMouseButton)))]
    `(case (.ordinal (.getButton ~e))
       ~@(apply concat pairs)
       ~fallback)))

(defn mousebtn-down? [^EventMouseButton evt]
  (.-_isPressed evt))

#_(defn mousedown-handler [{}]
    (fn [ctx rect evt]))

(defn scroll-dx [^EventMouseScroll evt]
  (.-_deltaX evt))

(defn scroll-dy [^EventMouseScroll evt]
  (.-_deltaY evt))

(def kw->event-class
  {:frame EventFrame
   :frame-skija EventFrameSkija
   :key EventKey
   :mouse-button EventMouseButton
   :mouse-move EventMouseMove
   :mouse-scroll EventMouseScroll
   :text-input EventTextInput
   :input-marked EventTextInputMarked
   :window-close EventWindowClose
   :window-close-req EventWindowCloseRequest
   :window-focus-in EventWindowFocusIn
   :window-focus-out EventWindowFocusOut
   :window-maximize EventWindowMaximize
   :window-minimize EventWindowMinimize
   :window-move EventWindowMove
   :window-resize EventWindowResize
   :window-restore EventWindowRestore
   :window-screen-change EventWindowScreenChange})

(defmacro case-event [evt & clauses]
  (let [fallback (when (odd? (count clauses))
                   (last clauses))
        pairs (mapcat (fn [[k expr]]
                        [(kw->event-class k) expr])
                (partitionv 2 clauses))]
    `(util/case-instance ~evt ~@pairs ~fallback)))

(defn key-down? [^EventKey evt]
  (.-_isPressed evt))

(defmacro mouse-x [evt]
  `(.-_x ~(vary-meta evt assoc :tag `EventMouseMove)))

(defmacro mouse-y [evt]
  `(.-_y ~(vary-meta evt assoc :tag `EventMouseMove)))

