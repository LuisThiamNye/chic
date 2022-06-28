(ns oclj.owned)

;; ownership enforcement:
;; - owners must take a key from an object and return it back when transferring ownership
;; - only one mutable reference OR multiple immutable references
;; do not allow mutation while immutable refs exist

;; wrapper around existing stateful objects

(comment
  (deftype Owned [obj ^int id]
    (borrow [_])
    (borrow-mut [_]))
  ;;
  )

;; macro like let-mutable to ensure safe use of contents

(defmacro with-mut [syms & body])

'(with-mut [owned1 owned2]
   ;; symbols rebound to inner values
   `(let [sym (do )]))

(defn disown [ctr])

(defn own [ctr])

;; what about lambda capturing variables? eg same lambda deployed multiple times in parallel
;; only safe for lambda to have immutable reference?
