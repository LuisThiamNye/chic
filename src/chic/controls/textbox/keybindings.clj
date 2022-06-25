(ns chic.controls.textbox.keybindings)

(def editor-keybindings
  [[:modifiers 'control
    [:keys
     'F :move-right
     'B :move-left
     'N :move-down
     'P :move-up
     'E :move-end
     'A :move-start
     'D :delete-right
     'K :kill
     'H :delete-left
     'W :delete-left-word]]
   [:modifiers :alt
    [:keys
     'right :move-right-word
     'left :move-left-word
     'backspace :delete-left-word]]
   [:keys
    'right :move-right
    'left :move-left
    'down :move-down
    'up :move-up
    'backspace :delete-left
    'delete :delete-right]
   [:modifiers 'shift
    [:keys
     'right :select-right
     'left :select-left]
    [:modifiers :alt
     [:keys
      'right :select-right-word
      'left :select-left-word]]
    [:modifiers :primary
     [:keys
      'right :select-end
      'left :select-start]]]
   [:modifiers :primary
    [:keys
     'A :select-all
     'right :move-end
     'left :move-start
     'backspace :delete-start
     'Z :undo]]])
