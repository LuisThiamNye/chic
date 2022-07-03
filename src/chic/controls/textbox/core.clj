(ns chic.controls.textbox.core
  (:require
   [proteus :refer [let-mutable]]
   [clojure.math :as math]
   [chic.controls.textbox.edit :as edit]
   [chic.controls.textbox.move :as move]
   [chic.controls.textbox.select :as select]
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

(def scale 2)
#_(def scale 1)

;; (defn textbox [])

(defn textbox-handle-keydown [*state evt]
  (when-some [intent (keybindings/evt->simple-keybindings-intent
                      keybindings/editor-keybindings-map evt)]
    (or (if (:select-idx @*state)
          (or (select/handle-deselecting-move-intent *state intent)
              (select/handle-selected-edit-intent *state intent))
          (or (move/handle-move-intent *state intent)
              (edit/handle-edit-intent *state intent)))
        (select/handle-select-intent *state intent))
    (hpr/-dbg-check-state @*state)))

(defn textbox-sample []
  (let [font (Font. style/face-code-default (* scale 14.))
        line-spacing (math/round (.getSpacing font))
        init-rope (.insert Rope/EMPTY 0 "Hello there.\nIt is me")
        *state (volatile! {:rope init-rope
                           :cursor-idx 0 ;; idx in rope
                           :cursor-line-idx 0 ;; which line
                           :cursor-dx 0
                           :select-idx nil
                           :line-start-idxs [0 13]
                           :text-lines []
                           :font font
                           ;; :line-end-idxs []
                           :dragging? false
                           :layout {:line-height line-spacing
                                    :first-line-origin (Point. 0. 0.)
                                    :first-line-x 0
                                    :text-plane-rect (Rect. 0. 0. 0. 0.)
                                    :line-end-xs []}})
        rope->textlines #(hpr/rope->textlines @*state %)
        _ (vswap! *state assoc :text-lines (rope->textlines init-rope))
        cursor-paint (huipaint/fill 0xE0007ACC)
        border-colour (unchecked-int 0x5f000000)
        border-colour-focused (unchecked-int 0xaf1a7dc2)
        border-paint ^Paint (huipaint/stroke border-colour scale)
        move-cursor-to-coord*
        (fn [state {:keys [x y]}]
          (let [{:keys [line-start-idxs text-lines]
                 {:keys [first-line-origin]} :layout} state
                dy (- y (:y first-line-origin))
                lidx (long (Math/floor (/ dy line-spacing)))
                nlines (count line-start-idxs)
                lidx (max 0 (min (dec nlines) lidx))
                text-line ^TextLine (nth text-lines lidx)
                dx (- x (:x first-line-origin))
                cidx (.getOffsetAtCoord text-line dx)]
            (-> state
                (assoc :cursor-dx (.getCoordAtOffset text-line cidx))
                (assoc :cursor-idx (+ (nth line-start-idxs lidx) cidx))
                (assoc :cursor-line-idx lidx))))]
    (ui2/attach-interactor
     {:cursor-style :ibeam
      :focus-node
      {:take-focus
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
         (vswap! *state edit/insert-text* text))}
      :on-mouse-down
      (fn [ctx rect evt]
        (ievt/case-mousebtn evt
          :primary
          (vswap! *state (fn [state]
                           (-> (move-cursor-to-coord* state (ui2/get-mouse-pos ctx))
                               (assoc :dragging? true)
                               (assoc :select-idx nil))))))
      :on-mouse-move
      (fn [ctx rect evt]
        (when (:dragging? @*state)
          (vswap! *state (fn [{:keys [select-idx cursor-idx] :as state}]
                           (let [state2 (move-cursor-to-coord* state (ui2/get-mouse-pos ctx))]
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
      (ui2/padded (* scale 0.5) (ui2/fill-rrect (* scale 2) border-paint))
      (ui2/padded
       (* scale 4)
       (ui2/watch-rect
        (fn --f1 [_ rect]
          (vswap! *state (fn [state]
                           (-> state
                               (assoc-in [:layout :first-line-origin] (Point. (:x rect) (:y rect)))
                               (assoc-in [:layout :text-plane-rect] rect)))))
        (ui2/stack
         ((ui2/direct-widget
           {:draw (fn [self ctx rect cnv]
                    (when (:select-idx @*state)
                      ((cursor/selction-draw-fn-for-layout
                        (let [{:keys [text-lines line-start-idxs cursor-line-idx
                                      cursor-idx select-idx] :as state} @*state
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
                          {:line-height line-spacing
                           :first-line-origin (update florigin :y + (* line-spacing line-idx1))
                           :first-line-x (+ flx (.getCoordAtOffset text-line1 local-idx1))
                           :line-end-xs
                           (if (== line-idx2 line-idx1)
                             [(+ flx (.getCoordAtOffset text-line1 local-idx2))]
                             (let [lf-width (* 4 scale)]
                               (-> [(+ flx lf-width (.getWidth text-line1))]
                                   (into (map #(+ flx lf-width (.getWidth ^TextLine %)))
                                         (subvec text-lines (inc line-idx1) line-idx2))
                                   (conj (+ flx (.getCoordAtOffset ^TextLine (nth text-lines line-idx2) local-idx2))))))})
                        (huipaint/fill 0xa0007ACC)) self ctx rect cnv)))}) {})
         (ui2/eqicolumn*
          line-spacing
          (fn [_]
            (mapv (fn [text-line]
                    (ui2/sized-with
                     (fn [_] (Point. 400. line-spacing))
                     (ui2/padded [0 0 0 (.getDescent (.getMetrics font))]
                                 (ui2/textline text-line))))
                  (:text-lines @*state))))
         #_(cursor-layer {:line-spacing line-spacing})
         (cursor/cursor-w
          {:getters {:paint (constantly (or cursor-paint #_(huipaint/fill 0x40a0a0a0)))
                     :line-height (fn [_] (:line-height (:layout @*state)))
                     :line-top (fn [_] (let [state @*state
                                             layout (:layout state)]
                                         (+ (:y (:text-plane-rect layout))
                                            (* (:cursor-line-idx state) (:line-height layout)))))
                     :cursor-x (fn [_] (+ (:x (:text-plane-rect (:layout @*state)))
                                          (:cursor-dx @*state)))}}))))))))

(comment
  (chic.windows/remount-all-windows)
  (alter-var-root #'scale (constantly 1))
  (alter-var-root #'scale (constantly 2))

  ;; TODO
  ;; - 6 word ops
  ;; - undo/redo
  ;; - when selecting whitespace, show a dot (at least for trailing whitespace)

#!
  )
;; short term goal: clojure notepad. Create small documents of clojure code.
;; code data can be accessed programmatically and also evaluated into vars
