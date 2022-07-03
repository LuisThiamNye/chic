(ns chic.controls.textbox.keybindings
  (:require
   [potemkin :refer [doit]]
   [chic.clj-editor.keybindings :as clj-editor.keybindings])
  (:import
   (io.github.humbleui.jwm EventKey Key)
   (io.lacuna.bifurcan IntMap)
   (java.util HashMap)))

(def editor-intents
  #{:move-left-word
    :move-left
    :move-right
    :move-right-word
    :move-up
    :move-down
    :move-start
    :move-start-up
    :move-end
    :move-end-down
    :move-doc-start
    :move-doc-end

    :select-left
    :select-left-word
    :select-right
    :select-right-word
    :select-start
    :select-start-up
    :select-end
    :select-end-down
    :select-doc-start
    :select-doc-end
    :select-all

    :delete-left
    :delete-left-word
    :delete-right
    :delete-start
    :delete-end

    :kill
    :yank
    :transpose
    :enter
    :undo
    :redo
    :copy
    :paste
    :cut})

(def editor-keybindings
  [[:mods nil
    ['right :move-right
     'left :move-left
     'down :move-down
     'up :move-up
     'backspace :delete-left
     'delete :delete-right
     'enter :enter]]
   [:mods 'control
    ['F :move-right
     'B :move-left
     'N :move-down
     'P :move-up
     'E :move-end
     'A :move-start
     'D :delete-right
     'K :kill
     'H :delete-left
     'W :delete-left-word
     'J :enter
     'T :transpose]]
   [:mods :alt
    ['right :move-right-word
     'left :move-left-word
     'down :move-end-down
     'up :move-start-up
     'backspace :delete-left-word
     'delete :delete-forward-word]]
   [:mods 'shift
    ['right :select-right
     'left :select-left
     'up :select-up
     'down :select-down]]
   [:mods 'shift :alt
    ['right :select-right-word
     'left :select-left-word
     'down :select-end-down
     'up :select-start-up]]
   [:mods 'shift :primary
    ['left :select-start
     'right :select-end
     'up :select-doc-start
     'down :select-doc-end
     'Z :redo]]
   [:mods :primary
    ['A :select-all
     'left :move-start
     'right :move-end
     'up :move-doc-start
     'down :move-doc-end
     'backspace :delete-start
     'delete :delete-end
     'X :cut
     'C :copy
     'V :paste
     'Z :undo]]])

(defn -simple-keybindings->map [spec]
  (let [mods->keys (.linear (IntMap.))]
    (letfn [(process [^IntMap keys->intent entry]
              (let [pairs (vec (partition 2 entry))]
                (doit [[ks intent] pairs]
                  (.put keys->intent (.ordinal (clj-editor.keybindings/keyspec->jwm ks))
                        intent))))]
      (doit [modspec spec]
        (let [mods (subvec (pop modspec) 1)
              mask (reduce + 0
                           (eduction (remove nil?)
                                     (map #(.-_mask (clj-editor.keybindings/modspec->jwm %)))
                                     mods))
              keys->intent (.linear (IntMap.))]
          (process keys->intent (peek modspec))
          (.put mods->keys mask (.forked keys->intent))))
      (.forked mods->keys))))

(def editor-keybindings-map (-simple-keybindings->map editor-keybindings))

(defn evt->simple-keybindings-intent [^IntMap mp evt]
  (let [evt ^EventKey (:jwm-event evt)
        k (.-_key evt)
        kmp ^IntMap (.get mp (cond-> (bit-and-not (.-_modifiers evt) 1)
                               (.isArrowKey k)
                               (bit-and-not 512)) nil)]
    (when kmp (.get kmp (.ordinal k) nil))))

;; use hashmaps with keys for each keys+modifiers combination for each kbd link
;; arbitrary modifiers should be allowed, but possibly optimise by firstly grouping by
;; the OS modifier bitmask.

;; important: trigger key so can't be done in reverse order

;; idea: ordered keybindings
;; - eg cmd+e+p+j different to cmd+p+e+j
;; advantage over spacemacs style: modeless, possibly less error prone
;;   also potentially faster (though in spacemacs you don't have to lift the keys if
;;   you are quick enough to avoid auto-repeat).
;; could be used in combination with vs-code style chords.
;; eg for holding down cmd+z, also press space (or enter etc) for instant repeat (more control over repeat)
;;   - and hold shift at any time to turn into redo
;;   - perhaps numbers / chrevrons / arrows / brackets can be used to control speed
;; in this way, keys can be both modifiers and triggers

;; keep key pressed state, preserving order (sorted set?)

;; when finding, use sets for keys of hash map to group the combinations
;; then for order-sensitive bindings, they can check the order of the trailing end of the sequence
;; (because order of things like cmd, shift should not matter)

;; caps lock largely ignored, treated as special since it has an OS-wide meaning and expectation.
;; you should rebind it anyway. It should be a state, not a normal key.
;; custom shortcuts can be used for custom modes, rather than abusing an existing, well-defined feature.
;; in any case, caps lock should not have a role in the composing of shortcuts as it does not act
;; like a normal key.
;; Though, when active, caps lock could alter the available keybindings.
;; It should be expected that the caps lock is used in isolation, between other keybindings.

;; there are so many different ways to do shortcuts/keybindings that it does not make sense to
;; enforce a particular model but rather just provide general hooks.
;; handlers report whether they handle an event, through the keydown.
;; keydown is necessary to put the handler into a transitory state to be receptive for actions that occur on keyup.
;;   eg user may be unloading lots of keys and would not want to trigger any action inadvertently.
;;   so release-actions should happen only if directly after the exact keydown combo.

;; text inputs should handle key events of all textual KeyTypes to prevent conflict with single-letter shortcuts
