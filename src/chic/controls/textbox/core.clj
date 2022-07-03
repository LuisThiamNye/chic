(ns chic.controls.textbox.core
  (:require
   [proteus :refer [let-mutable]]
   [clojure.math :as math]
   [chic.controls.textbox.edit :as edit]
   [chic.controls.textbox.move :as move]
   [chic.controls.textbox.select :as select]
   [chic.controls.textbox.undo :as undo]
   [chic.controls.textbox.helper :as hpr]
   [chic.clipboard :as clipboard]
   [chic.debug]
   [taoensso.encore :as enc]
   [chic.controls.textbox.cursor :as cursor]
   [potemkin :refer [doit]]
   [chic.controls.textbox.keybindings :as keybindings]
   [chic.style :as style]
   [chic.ui2.event :as ievt]
   [chic.ui.font :as uifont]
   [chic.ui :as cui]
   [chic.ui.ui2 :as ui2]
   [chic.clj-editor.ast :as ast]
   [io.github.humbleui.paint :as huipaint]
   [chic.paint :as cpaint]
   [chic.util :as util]
   [chic.ui.layout :as cuilay]
   [io.github.humbleui.ui :as ui]
   [chic.clj-editor.parser :as parser])
  (:import
   (io.lacuna.bifurcan Rope)
   (io.github.humbleui.skija Paint Font Canvas TextLine)
   (io.github.humbleui.types Rect Point RRect)))

;; (defn textbox [])

;; (defn init-state* [])

(defn textbox-handle-keydown [*state evt]
  (when-some [intent (keybindings/evt->simple-keybindings-intent
                      keybindings/editor-keybindings-map evt)]
    (let [state1 @*state]
      (undo/maybe-save-undo *state)
      (or (if (:select-idx state1)
            (or (select/handle-deselecting-move-intent *state intent)
                (select/handle-selected-edit-intent *state intent))
            (or (move/handle-move-intent *state intent)
                (edit/handle-edit-intent *state intent)))
          (select/handle-select-intent *state intent)
          (case intent
            :undo (vswap! *state undo/perform-undo*)
            :redo (vswap! *state undo/perform-redo*)
            nil))
      (vswap! *state (fn [state2]
                       (if (and (== (:cursor-line-idx state2) (:cursor-line-idx state1))
                                (not (== (:cursor-idx state2) (:cursor-idx state1))))
                         (assoc state2 :cursor-target-dx (:cursor-dx state2))
                         state2))))
    (hpr/-dbg-check-state @*state)))

(defn bare-textarea [*state {:keys [selection-paint cursor-paint]}]
  (ui2/attach-interactor
   {:cursor-style :ibeam
    :focus-node
    {#_#_#_#_:take-focus
           (fn []
             (.setColor border-paint border-colour-focused)
             true)
         :release-focus
       (fn []
         ;; todo: cancel keydown state
         (.setColor border-paint border-colour)
         true)
       ;; :keybindings nil
     :handle-keydown #(textbox-handle-keydown *state %)
     :on-text-input
     (fn [^String text]
       (undo/maybe-save-undo *state)
       (vswap! *state edit/insert-text* text))}
    :on-mouse-down
    (fn [ctx rect evt]
      (ievt/case-mousebtn evt
        :primary
        (vswap! *state (fn [state]
                         (-> (move/move-cursor-to-coord* state (ui2/get-mouse-pos ctx))
                             (assoc :dragging? true)
                             (assoc :select-idx nil))))))
    :on-mouse-move
    (fn [ctx rect evt]
      (when (:dragging? @*state)
        (vswap! *state (fn [{:keys [select-idx cursor-idx] :as state}]
                         (let [state2 (move/move-cursor-to-coord* state (ui2/get-mouse-pos ctx))]
                           (cond-> state2
                             (not (== cursor-idx (:cursor-idx state2)))
                             (assoc :select-idx (when-not (= select-idx (:cursor-idx state2))
                                                  (if (nil? select-idx)
                                                    cursor-idx
                                                    select-idx)))))))))
    :on-mouse-up (fn [ctx _rect evt]
                   (ievt/case-mousebtn evt
                     :primary
                     (vswap! *state (fn [state]
                                      (assoc state :dragging? false)))))
    :on-scroll (fn [evt])}
   (ui2/stack
    (ui2/padded
     4
     (ui2/watch-rect
      (fn --f1 [_ rect]
        (vswap! *state (fn [state]
                         (-> state
                             (assoc-in [:layout :first-line-origin] (Point. (:x rect) (:y rect)))
                             (assoc-in [:layout :text-plane-rect] rect)))))
      (ui2/stack
       ((ui2/direct-widget
         {:draw (fn [self {:keys [scale] :as ctx} rect cnv]
                  (when (:select-idx @*state)
                    ((cursor/selction-draw-fn-for-layout
                      (let [{:keys [text-lines line-start-idxs cursor-line-idx
                                    cursor-idx select-idx]
                             {:keys [line-height]} :layout :as state} @*state
                            sel-line-idx (hpr/find-line-idx line-start-idxs select-idx)
                            rev? (< cursor-idx select-idx)
                            line-idx1 (if rev? cursor-line-idx sel-line-idx)
                            line-idx2 (if rev? sel-line-idx cursor-line-idx)
                            local-idx1 (- (if rev? cursor-idx select-idx)
                                          (nth line-start-idxs line-idx1))
                            local-idx2 (- (if rev? select-idx cursor-idx)
                                          (nth line-start-idxs line-idx2))
                            text-line1 ^TextLine (nth text-lines line-idx1)
                            florigin (:first-line-origin (:layout state))
                            flx (:x florigin)]
                        {:line-height line-height
                         :first-line-origin (update florigin :y + (* line-height line-idx1))
                         :first-line-x (+ flx (.getCoordAtOffset text-line1 local-idx1))
                         :line-end-xs
                         (if (== line-idx2 line-idx1)
                           [(+ flx (.getCoordAtOffset text-line1 local-idx2))]
                           (let [lf-width (* 4 scale)]
                             (-> [(+ flx lf-width (.getWidth text-line1))]
                                 (into (map #(+ flx lf-width (.getWidth ^TextLine %)))
                                       (subvec text-lines (inc line-idx1) line-idx2))
                                 (conj (+ flx (.getCoordAtOffset ^TextLine (nth text-lines line-idx2) local-idx2))))))})
                      selection-paint) self ctx rect cnv)))}) {})
       (ui2/eqicolumn*
        (fn [_] (:line-height (:layout @*state)))
        (fn [_]
          (let [{:keys [^Font font]
                 {:keys [line-height]} :layout} @*state]
            (mapv (fn [text-line]
                   (ui2/sized-with
                    (fn [_] (Point. 400. line-height))
                    (ui2/padded-unscaled [0 0 0 (.getDescent (.getMetrics font))]
                                         (ui2/textline text-line))))
                 (:text-lines @*state)))))
       (cursor/cursor-w
        {:getters {:paint (constantly cursor-paint)
                   :line-height (fn [_] (:line-height (:layout @*state)))
                   :line-top (fn [_] (let [state @*state
                                           layout (:layout state)]
                                       (+ (:y (:text-plane-rect layout))
                                          (* (:cursor-line-idx state) (:line-height layout)))))
                   :cursor-x (fn [_] (+ (:x (:text-plane-rect (:layout @*state)))
                                        (:cursor-dx @*state)))}})))))))

(defn textbox-sample []
  (let [init-rope (.insert Rope/EMPTY 0 "Hello there.\nIt is me.\nIt is good\nLife is great.\nToes.\nNose\nGear")
        *state (volatile! nil)
        cursor-paint (huipaint/fill 0xE0007ACC)
        border-colour (unchecked-int 0x5f000000)
        border-colour-focused (unchecked-int 0xaf1a7dc2)
        selection-paint (huipaint/fill 0xa0007ACC)
        border-paint ^Paint (huipaint/stroke border-colour 0)]
    (ui2/on-mount
     (fn [{:keys [scale]}]
       (.setStrokeWidth border-paint (float scale))
       (let [font (Font. style/face-code-default (* scale 14.))]
         (vreset! *state
                  (hpr/recalculate-derived*
                   {:rope init-rope
                    :cursor-idx 0 ;; idx in rope
                    :cursor-line-idx nil ;; which line
                    :cursor-dx nil
                    :cursor-target-dx nil
                    :select-idx nil
                    :line-start-idxs nil
                    :text-lines nil
                    :font font
                    :font-size 14.
                    ;; :line-end-idxs []
                    :dragging? false
                    :last-undo-commit-time 0
                    :undo-commit-idx -1
                    :undo-commits []
                    :layout {:line-height (math/round (.getSpacing font))
                             :first-line-origin (Point. 0. 0.)
                             :first-line-x 0
                             :text-plane-rect (Rect. 0. 0. 0. 0.)
                             #_#_:line-end-xs []}}))
         (vswap! *state assoc :text-lines (hpr/rope->textlines @*state init-rope))))
     (ui2/stack
      (ui2/padded 0.5 (ui2/fill-rrect 2 border-paint))
      (bare-textarea *state {:selection-paint selection-paint :cursor-paint cursor-paint})))))

(comment
  (chic.windows/remount-all-windows)

  ;; TODO
  ;; - undo/redo - track the number of characters changed?
  ;; - when selecting whitespace, show a dot (at least for trailing whitespace)

  ;; also see https://github.com/jhallen/joes-sandbox/tree/master/editor-perf
  ;; also inspiration: sublime text python API
  ;;    - has 1+ regions of start/end of selection to define cursors

  ;; simplified theory of sublime's undo: snapshot taken after 3 seconds since last edit,
  ;; but certain actions can reset the timer and force a snapshot

  ;; garbage collection of undo steps could be random, following exp distribution

#!
  )
;; short term goal: clojure notepad. Create small documents of clojure code.
;; code data can be accessed programmatically and also evaluated into vars
