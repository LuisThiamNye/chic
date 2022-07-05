(ns chic.ui.interactor
  (:import
   (io.github.humbleui.types Rect)
   (io.github.humbleui.jwm Window Event EventMouseButton EventMouseScroll
                           EventTextInput EventMouseMove EventKey
                           MouseCursor)))

#_(defn standard-button [{:keys [no-hover? on-click]}]
  [[:cursor-rect
    {:rect :component}]]
  (fn [ctx rect]))

#_(defn interactable [interactors geometry])

(def kw->mouse-cursor-style
  {:arrow MouseCursor/ARROW
   :crosshair MouseCursor/CROSSHAIR
   :help MouseCursor/HELP
   :pointing-hand MouseCursor/POINTING_HAND
   :ibeam MouseCursor/IBEAM
   :forbidden MouseCursor/NOT_ALLOWED
   :wait MouseCursor/WAIT
   :resize-v MouseCursor/RESIZE_NS
   :resize-h MouseCursor/RESIZE_WE
   :resize-tr MouseCursor/RESIZE_NESW
   :resize-lt MouseCursor/RESIZE_NWSE
   :draggable nil ; mac: open hand; windows: sizeall
   :dragging nil ; mac: closed hand; windows: sizeall
   })

(defn ^Window mgr-get-window [mgr]
  (:jwm-window @mgr))

;; (defn mgr-get-focused-intr [mgr])

(defn mgr-get-hovered-intr [mgr]
  (get (:intrs @mgr) (:hovered-intr-id @mgr)))

#_(defn mgr-set-hovered-intr [mgr intr]
  (.setMouseCursor (mgr-get-window mgr)
                   (:cursor-style-default intr MouseCursor/ARROW)))

(defn -find-intr-at-point [mgr x y]
  (some (fn [intr]
          (let [rect ^Rect (:rect intr)]
            (when (and (<= (.getLeft rect) x)
                       (<= (.getTop rect) y)
                       (< x (.getRight rect))
                       (< y (.getBottom rect)))
              intr)))
        (vals (:intrs @mgr))))

(defn mgr-notify-mouse-pos [mgr ctx ^EventMouseMove event]
  (let [x (.getX event)
        y (.getY event)
        intr (-find-intr-at-point mgr x y)]
    (vswap! mgr assoc :hovered-intr-id (:id intr))
    ;; (when (.contains (.getContentRect ^Window (:jwm-window @mgr)) (int x) (int y)))
    (.setMouseCursor ^Window (:jwm-window @mgr) (:cursor-style intr MouseCursor/ARROW))
    (when-some [f (:on-mouse-move intr)]
      (f ctx (:rect intr) event))))

(defn mgr-notify-intr-scroll [mgr ctx event]
  (let [mouse-pos (:chic.ui.ui2/mouse-pos ctx)
        intr (-find-intr-at-point mgr (:x mouse-pos) (:y mouse-pos))]
    (when-some [f (:on-scroll intr)]
      (f ctx (:rect intr) event))))

(defn mgr-notify-intr-mousedown [mgr ctx event]
  (let [mouse-pos (:chic.ui.ui2/mouse-pos ctx)
        intr (-find-intr-at-point mgr (:x mouse-pos) (:y mouse-pos))]
    (when (not (when-some [f (-> intr :focus-node :take-focus)]
                 (not (f))))
      (vswap! mgr assoc :focused-intr-id (:id intr)))
    (when-some [f (:on-mouse-down intr)]
     (f ctx (:rect intr) event))))

(defn mgr-notify-intr-mouseup [mgr ctx event]
  (let [;mouse-pos (:chic.ui.ui2/mouse-pos ctx)
        intr (get (:intrs @mgr) (:focused-intr-id @mgr))]
    (when-some [f (:on-mouse-up intr)]
      (f ctx (:rect intr) event))))

(defn mgr-notify-intr-textinput [mgr ctx ^EventTextInput event]
  (let [intr (get (:intrs @mgr) (:focused-intr-id @mgr))]
    (when-some [f (:on-text-input (:focus-node intr))]
      (f (.getText event)))))

(defn mgr-create-intr [mgr opts]
  (let [id (random-uuid)]
    (vswap! mgr update :intrs assoc id
            (-> opts
                (assoc :id id)
                (assoc :cursor-style
                       (kw->mouse-cursor-style (:cursor-style opts :arrow)))))
    id))

(defn mgr-intr-set-rect [mgr intr-id rect]
  (vswap! mgr update :intrs update intr-id assoc :rect rect))

(defn new-mgr [{:keys [jwm-window]}]
  (volatile! {:hovered-intr-id nil
              :focused-intr-id nil
              :jwm-window jwm-window
              :intrs {}}))

(defn mgr-handle-jwm-event [mgr ctx ^Event event]
  (condp instance? event
    EventMouseMove
    (mgr-notify-mouse-pos mgr ctx event)
    EventMouseScroll
    (mgr-notify-intr-scroll mgr ctx event)
    EventMouseButton
    (if (.isPressed ^EventMouseButton event)
      (mgr-notify-intr-mousedown mgr ctx event)
      (mgr-notify-intr-mouseup mgr ctx event))
    EventKey
    (let [event ^EventKey event
          intr (get (:intrs @mgr) (:focused-intr-id @mgr))]
      (when (.-_isPressed event)
        (when-some [f (:handle-keydown (:focus-node intr))]
         (f {:jwm-event event :pressed-keys #{"TODO"}}))))
    EventTextInput
    (mgr-notify-intr-textinput mgr ctx event)
    nil))

"Event types
Mouse:
- move
- button
- scroll
Keyboard:
- key
- textinput
Window:
- focus
- resize
- move
- maximise/minimise/restore
- screen-change
"

(comment
  (chic.windows/dosendui
   (.setMouseCursor (:window-obj (first (vals @chic.windows/*windows)))
                    ;; MouseCursor/IBEAM
                    ;; MouseCursor/ARROW
                    MouseCursor/CROSSHAIR
                    ))
  (.-_lastCursor (:window-obj (first (vals @chic.windows/*windows))))
  #!
  )
