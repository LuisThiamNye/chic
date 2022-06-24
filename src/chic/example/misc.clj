(ns chic.example.misc)

;; framerate counter
()

;; bouncing spash text
#_(defui splash-label
  (ui/rotate-within
   30
   (ui/align-centre
    (ui/scale
     $text-scale
     (ui/shadow
      {:dx 5 :dy 5 :blur 0 :spread 0}
      (ui/label $splash-text
                (font "Input Mono" :weight :bold)
                (fill (okhsv 0.3 0.9 0.9))))))))

#_(defui bouncing-splash
  (ui/bind
   {text-scale 1}
   (ui/animate-loop
    {:range [0 math/FULL_ROT]
     :duration 1000
     :f (fn [t] (set! text-scale (inc (* 0.25 (dec (math/cos t))))))}
    (splash-label {$text-scale text-scale
                   $splash-text "Hello there"}))))


;; simple checkbox
(defui simple-checkbox
  (ui/row
   (ui/square
    (ui/guides
     {:get {:keys [width rect]}
      :bind [inset (max 1 (* 0.05 width))
             inner-rect (shrink-rect rect :all4 inset)
             inner-rrect (rect->rrect inner-rect :all4 [:% 30])]}
     (ui/canvas
      (draw-rrect-border-out inner-rrect inset (fill (grey 0.8)))
      (draw-rrect inner-rrect (if $checked?
                                (fill (okhsv 0.6 0.9 0.9))
                                (fill :white)))
      (when $checked?
        (draw-path "..." (fill :white))))))
   (when $text
     (ui/label $text))))

;; button
;; checkbox
;; combo box
;; radio button
;; text field
;; tabs
;; active button
;; toolbar button
;; stepper control
;; general slider (circle)
;; discrete slider with scale (pill)
;; progress bar
;; indeterminate loading bar
;; loading circle
;; indeterminate loading circle

;; button that has an enlarged hitbox once held down (like macOS)
