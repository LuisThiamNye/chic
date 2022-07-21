(ns oclj.core)

(comment
  (deftype _ [])

  #!
  )

;; give ways to implement iassociative and other interfaces


;; model:
;; generic instance object wraps an inner object of a specific class containing the fields
;; one field for version
;; when upgrading, swap out the inner fields object

;; other idea: only swap out a method/fields if its API is compatible with the object
