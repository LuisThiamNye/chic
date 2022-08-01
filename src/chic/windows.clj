(ns chic.windows
  (:require
   [potemkin :as pot :refer [doit]]
   [chic.debug :as debug]
   [chic.util :as util :refer [<- inline<-]]
   [taoensso.encore :as enc]
   [chic.ui.event :as uievt]
   [io.github.humbleui.paint :as huipaint]
   [chic.protocols]
   [chic.error :as error]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.style :as style]
   [chic.ui :as cui]
    [chic.ui2.event :as ievt]
    [chic.ui.canvas :as cnv]
   [chic.ui.error :as cui.error]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.protocols :as huip]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin])
  (:import
    (java.lang AutoCloseable)
   (io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll
     EventKey EventWindowFocusOut App Window EventWindowResize EventTextInput
     EventFrame Platform Event EventWindowClose EventWindowCloseRequest)
   (io.github.humbleui.jwm.skija 
     EventFrameSkija LayerD3D12Skija LayerGLSkija LayerMetalSkija)
   (io.github.humbleui.skija Canvas Font Paint)
   (io.github.humbleui.types IPoint)))

(defmacro on-ui-thread? []
  `(App/_onUIThread))

(defmacro dosendui [& body]
  `(do
     (App/runOnUIThread #(try ~@body (catch Throwable t# nil)))
     nil))

(defmacro safe-dosendui [& body]
  `(let [f# #(try ~@body (catch Throwable t# nil))]
     (if (App/_onUIThread)
       (f#)
       (App/runOnUIThread f#))
     nil))

(defmacro safe-doui-async [& body]
  `(let [p# (promise)
         f# #(deliver p# (try ~@body (catch Throwable t# t#)))]
     (if (App/_onUIThread) ;; doui causes freeze if already on UI
       (f#)
       (App/runOnUIThread f#))
     p#))

(defmacro safe-doui [& body]
  `(let [res# (deref (safe-doui-async ~@body))]
     (if (instance? Throwable res#)
       (throw res#)
       res#)))

(defn request-frame [{:keys [window-obj]}]
  {:pre [(instance? io.github.humbleui.jwm.Window window-obj)]}
  (safe-dosendui (huiwin/request-frame window-obj)))

(defn activate-window [{:keys [^Window window-obj]}]
  {:pre [(instance? io.github.humbleui.jwm.Window window-obj)]}
  (safe-dosendui (.focus window-obj)))

(defonce *windows (atom {}))

(declare make)

(defn make-render-error-window [{:keys [throwable] :as opts}]
  (let [screen (last (hui/screens))
        scale (:scale screen)
        area (:work-area screen)
        width (* 600 scale)
        height (int (* 0.9 (:height area)))
        x (-> (:width area) (- width) (/ 2) (+ (:x area)))
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
     (make
      {:id (random-uuid)
       :build-app-root (fn [] (cui/dyncomp (error/build-render-error-window-root opts)))
       :on-close (fn [])})
      (huiwin/set-title (str "Render error: " throwable))
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(defn window-app-rect [window-obj]
  (cui/rect-with-wh (huiwin/content-rect window-obj)))

(defn send-event [{:keys [window-obj *ctx *app-root] :as w} event]
  (huip/-event
   @*app-root (enc/merge @*ctx
                         (assoc (assoc event :chic/current-window w) :chic.ui/component-rect
                                (window-app-rect window-obj)))))

(defn schedule-after-event [event callback]
  (when-some [f (::event.schedule-after event)]
    (f callback)))

(def ^ThreadLocal *root-ctx (ThreadLocal.))

(defn on-event-handler [{:keys [window-obj *ctx] :as win} jwmevt]
  (try
    (profile/reset "event")
    (profile/measure
     "event"
     (let [post-handlers (java.util.ArrayList. 0)
           sae (fn [callback] (.add post-handlers callback))
           send-event (fn [win event]
                        (send-event win (assoc event ::event.schedule-after sae
                                               :raw-event jwmevt)))
           changed?
           (condp instance? jwmevt
             EventMouseMove
             (let [pos (IPoint. (.getX ^EventMouseMove jwmevt) (.getY ^EventMouseMove jwmevt))
                   event {:hui/event :hui/mouse-move
                          :chic.ui/mouse-win-pos pos
                          ;; :hui.event/pos pos
                          }]
               (.set *root-ctx (swap! *ctx assoc :chic.ui/mouse-win-pos pos))
               (send-event win event))

             EventMouseButton
             (let [event {:hui/event :hui/mouse-button
                          :event jwmevt
                          :hui.event.mouse-button/is-pressed (.isPressed ^EventMouseButton jwmevt)}]
               (send-event win event))

             EventMouseScroll
             (send-event win
                         {:hui/event :hui/mouse-scroll
                          :hui.event.mouse-scroll/dx (.getDeltaX ^EventMouseScroll jwmevt)
                          :hui.event.mouse-scroll/dy (.getDeltaY ^EventMouseScroll jwmevt)})

             EventWindowFocusOut
             (send-event win
                         {:hui/event :hui/window-focus-out})

             EventKey
             (send-event win
                         {:hui/event (if (.isPressed ^EventKey jwmevt) :hui/key-down :hui/key-up)
                          :hui.event.key/key (.getName (.getKey ^EventKey jwmevt))
                          :eventkey jwmevt})

             EventTextInput
             (do
               (send-event win
                           {:hui/event :hui/text-input
                            :hui.event.text-input/text (.getText ^EventTextInput jwmevt)}))

             EventWindowResize
             true
             (do #_(println "Other event:" (type event)) nil))]
       (doit [cb post-handlers]
         (cb))
       (when (or changed? (not (or (instance? EventFrame jwmevt)
                                 (instance? EventWindowClose jwmevt)
                                 (instance? EventWindowCloseRequest jwmevt)
                                   (instance? EventFrameSkija jwmevt)
                                   ;(instance? EventMouseMove jwmevt)
                                   ;(instance? EventKey jwmevt)
                                   ;; (instance? EventMouseButton jwmevt)
                                   #_(instance? EventTextInput jwmevt))))
         ;; (vswap! (:*profiling win) assoc :event-triggers-change-time (System/nanoTime))
         (huiwin/request-frame window-obj))))
    (catch Throwable e
      (debug/println-main (pr-str e)))
    (finally
      #_(profile/log "event"))))

(defn remount-window [window]
  (ui/child-close @(:*app-root window))
  (vreset!
   (:*app-root window)
   ((:build-app-root window)))
  (doseq [window (vals @*windows)]
    (huiwin/request-frame (:window-obj window))))

(defn set-visible [window tf]
  (huiwin/set-visible (:window-obj window) tf))

(defn error-view []
  (ui/dynamic
   ctx [{:keys [scale]} ctx]
   (let [font-ui (Font. style/face-code-default (float (* scale 13)))
         fill-text (huipaint/fill 0xFF000000)]
     (ui/with-context
       {:font-ui font-ui
        :fill-text fill-text}
       (ui/dynamic
        ctx [{:keys [*ui-error] :as window} (:chic/current-window ctx)
             ui-error @*ui-error]
        (let [{:keys [throwable bitmap]} ui-error]
          (ui/fill
           (huipaint/fill 0xFFF6E6E6)
           (cuilay/vscrollbar
            (cuilay/vscroll
             (cuilay/column
              (cuilay/padding
               5 5
               (cuilay/column
                (cui/clickable
                 (uievt/on-primary-down (fn [_] (remount-window window)))
                 (ui/fill (huipaint/fill 0x11000000)
                          (cuilay/halign
                           0.5 (cuilay/padding 20 5 (ui/label "Reload window" font-ui fill-text)))))
                (cuilay/padding 5 5 (ui/label (str throwable) font-ui fill-text))
                (cui.error/bound-errors
                 (cuilay/column
                  (when bitmap
                    (cui/dyncomp (error/partial-canvas-preview bitmap)))
                  (cui/dyncomp (error.stacktrace/stack-trace-view throwable))
                  (cui/dyncomp (error/full-error-view-of throwable))))))))))))))))

(def *always-render? (volatile! false))

(defn on-paint-handler [{:keys [*app-root window-obj *ctx *ui-error] :as w} ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF6F6F6))
  (let [bounds (window-app-rect window-obj)
        scale (huiwin/scale window-obj)
        ctx (swap! *ctx
                   (fn [ctx]
                     (enc/merge
                      (assoc (util/assoc-if-not= ctx :scale scale)
                             :chic/current-window w
                             :chic.profiling/time-since-last-paint
                             (unchecked-subtract (System/nanoTime) (:paint-start-time @(:*profiling w)))
                             :chic.ui/component-rect bounds
                             :chic.ui/window-content-bounds bounds
                             :chic.error/make-render-error-window make-render-error-window)
                      (style/context-default {:scale scale}))))]
    (profile/reset)
    (vswap! (:*profiling w) assoc :paint-start-time (System/nanoTime))
    (profile/measure
     "frame"
     (try (.set *root-ctx ctx)
          (huip/-draw @*app-root ctx bounds canvas)
          (catch Throwable e
            (vreset! *ui-error {:bitmap (error/canvas->bitmap canvas bounds)
                                :throwable e
                                :method :draw
                                :ctx ctx
                                :cs bounds})
            (.clear canvas (unchecked-int 0xFFF6F6F6))
            (vreset! *app-root (cui/dyncomp (error-view)))
            (huip/-draw @*app-root ctx bounds canvas))
          (finally
            (.remove *root-ctx))))
    (vswap! (:*profiling w) assoc :latest-paint-duration
            (unchecked-subtract (System/nanoTime) (:paint-start-time @(:*profiling w))))
    #_(enc/after-timeout 1000 (request-frame w))
    (when @*always-render?
      (request-frame w))
    #_(let [{:keys [paint-start-time event-triggers-change-time paint-done-time]} @(:*profiling w)]
        (chic.debug/println-main
         "(ms)"
         "change->paint:" (/ (float (- paint-start-time event-triggers-change-time)) 1000000)
         "paint->done: " (/ (float (- paint-done-time paint-start-time)) 1000000)))
    #_(profile/log)))

(defn make [{:keys [id on-close *app-root on-paint on-event *ctx build-app-root] :as opts}]
  {:pre [(on-ui-thread?)]}
  (let [on-paint (or on-paint #'on-paint-handler)
        on-event (or on-event #'on-event-handler)
        *app-root (or *app-root (volatile! nil))
        w {:id id
           :*app-root *app-root
           :*ui-error (volatile! nil)
           :*profiling (volatile! {:paint-start-time 0
                                   :latest-paint-duration 0})
           :build-app-root build-app-root
           ;; :*visible? (volatile! false)
           :*ctx (or *ctx (atom {}))
           :window-obj (huiwin/make
                        {:on-close #(do (swap! *windows dissoc id)
                                      (ui/child-close @*app-root)
                                      (on-close))
                         :on-paint #(on-paint (get @*windows id) %2)
                         :on-event #(on-event (get @*windows id) %2)})}
        w (with-meta w
            {`chic.protocols/request-frame #'request-frame})]
    (vreset! *app-root (build-app-root))
    (swap! *windows assoc id w)
    (:window-obj w)))

(defn make2 [{:keys [id on-close *app-root on-paint on-event *ctx build-app-root] :as opts}]
  {:pre [(on-ui-thread?)]}
  (let [on-paint (or on-paint #'on-paint-handler)
        on-event (or on-event #'on-event-handler)
        on-close (or on-close (constantly nil))
        *app-root (or *app-root (volatile! nil))
        id (or id (random-uuid))
        w {:id id
           :*app-root *app-root
           :*ui-error (volatile! nil)
           :*profiling (volatile! {:paint-start-time 0
                                   :latest-paint-duration 0})
           :build-app-root build-app-root
           ;; :*visible? (volatile! false)
           :*ctx (or *ctx (atom {}))
           :window-obj (huiwin/make
                        {:on-close #(do (swap! *windows dissoc id)
                                      (ui/child-close @*app-root)
                                      (on-close))
                         :on-paint #(on-paint (get @*windows id) %2)
                         :on-event #(on-event (get @*windows id) %2)})}
        w (with-meta w
            {`chic.protocols/request-frame #'request-frame})]
    (vreset! *app-root (build-app-root))
    (swap! *windows assoc id w)
    w))

(definterface PaintAndEventHandler
  (^void paint [^io.github.humbleui.skija.Canvas canvas])
  (^void notifyEvent [^io.github.humbleui.jwm.Event _event]))

(defn set-event-listener [^Window jw ^PaintAndEventHandler handler]
  (.setEventListener jw
    (reify java.util.function.Consumer
      (accept [_ evt]
        (ievt/case-event evt
          :frame-skija
          (let [cnv (.getCanvas (.getSurface evt))]
            (cnv/with-save cnv
              (try (.paint handler cnv)
                (catch Throwable e
                  (.printStackTrace e)))))
          (.notifyEvent handler evt))))))

(defn make-jwm-window
  (^Window []
    (doto (App/makeWindow)
      (.setLayer
        (util/case-enum Platform/CURRENT
          WINDOWS (LayerD3D12Skija.)
          MACOS (LayerMetalSkija.)
          X11 (LayerGLSkija.)))))
  (^Window [^PaintAndEventHandler handler]
    (doto (make-jwm-window)
      (set-event-listener handler))))

(definterface RenewableWindow
  (renew []))

(def *window-registry (atom #{}))
(comment (count @*window-registry)
  (.close (:jwm-window (first @*window-registry)))
  )

(defn register-window! [win]
  (swap! *window-registry conj win))

(defn unreg-window! [win]
  (swap! *window-registry disj win))

(defn set-window-rect
  "Dimensions must be int"
  [^Window jw rect]
  (when rect
    (.setWindowSize jw (:width rect) (:height rect))
    (.setWindowPosition jw (:x rect) (:y rect)))
  jw)

(defn remount-all-windows []
  (safe-dosendui
    (doseq [w (vals @*windows)]
      (remount-window w))
    (doseq [w @*window-registry]
      (when (instance? RenewableWindow w)
        (swap! *window-registry conj 
          (.renew ^RenewableWindow w))
        (when (instance? AutoCloseable w)
          (.close ^AutoCloseable w))))))

(comment
  (remount-window (val (first @*windows)))
  (request-frame (val (first @*windows)))

  (-> (debug/obj->clj (first @*window-registry))
    (get 'root)
    debug/obj->clj
    (get 'dev_view)
    debug/obj->clj
    (->> (into {}))
    clojure.pprint/pprint)

  #!
  )
