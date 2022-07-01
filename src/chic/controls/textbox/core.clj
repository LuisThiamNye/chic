(ns chic.controls.textbox.core
  (:require
   [proteus :refer [let-mutable]]
   [chic.controls.textbox.edit :as edit]
   [chic.controls.textbox.move :as move]
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
    (or (move/handle-move-intent *state intent)
        (edit/handle-edit-intent *state intent))
    (hpr/-dbg-check-state @*state)))

(defn textbox-sample []
  (let [font (Font. style/face-code-default (* scale 14.))
        line-spacing (.getSpacing font)
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
        border-paint ^Paint (huipaint/stroke border-colour 1)]
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
         (vswap! *state edit/insert-text*))}
      :on-mouse-down
      (fn [ctx rect evt]
        (ievt/case-mousebtn evt
          :primary
          (let [{:keys [line-start-idxs text-lines]
                 {:keys [first-line-origin]} :layout} @*state
                {:keys [x y]} (ui2/get-mouse-pos ctx)
                dy (- y (:y first-line-origin))
                lidx (long (Math/floor (/ dy line-spacing)))
                nlines (count line-start-idxs)
                lidx (max 0 (min (dec nlines) lidx))
                text-line ^TextLine (nth text-lines lidx)
                dx (- x (:x first-line-origin))
                cidx (.getOffsetAtCoord text-line dx)]
            (vswap! *state (fn [state]
                             (-> state
                                 (assoc :cursor-dx (.getCoordAtOffset text-line cidx))
                                 (assoc :cursor-idx (+ (nth line-start-idxs lidx) cidx))
                                 (assoc :cursor-line-idx lidx)))))))
      :on-mouse-up (fn [evt])
      :on-scroll (fn [evt])}
     (ui2/stack
      (ui2/padded 0.5 (ui2/fill-rrect (* scale 2) border-paint))
      (ui2/padded
       (* scale 4)
       (ui2/watch-rect
        (fn --f1 [_ rect]
          (vswap! *state (fn [state]
                           (-> state
                               (assoc-in [:layout :first-line-origin] (Point. (:x rect) (:y rect)))
                               (assoc-in [:layout :text-plane-rect] rect)))))
        (ui2/stack
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
          {:getters {:paint (constantly cursor-paint)
                     :line-height (fn [_] (:line-height (:layout @*state)))
                     :line-top (fn [_] (let [state @*state
                                             layout (:layout state)]
                                         (+ (:y (:text-plane-rect layout))
                                            (* (:cursor-line-idx state) (:line-height layout)))))
                     :cursor-x (fn [_] (+ (:x (:text-plane-rect (:layout @*state)))
                                          (:cursor-dx @*state)))}})
         #_((ui2/direct-widget
             {:draw (cursor/selction-draw-fn-for-layout
                     {:line-height 30.
                      :first-line-origin (Point. 105. 40.)
                      :first-line-x 300.
                      :line-end-xs [500. 300. 311.]}
                     (huipaint/fill 0xa0007ACC))}) {}))))))))

(comment
  (chic.windows/remount-all-windows)
  (alter-var-root #'scale (constantly 1))
  (alter-var-root #'scale (constantly 2))

  ;; interactor
  ;; mouse areas

#!
  )
;; short term goal: clojure notepad. Create small documents of clojure code.
;; code data can be accessed programmatically and also evaluated into vars
