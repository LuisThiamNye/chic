(ns chic.util.rect
  (:require
    [potemkin :refer [doit]]
    [chic.util.typed :as util.typed]
    [chic.util :refer [<-]]
    [taoensso.encore :as enc])
  (:import
    (io.github.humbleui.types Rect RRect IRect Point IPoint)))

; (defmacro let-dims [])

(doit [cls [Rect IRect RRect]]
  (util.typed/register-lookup-type! cls
    (reify chic.util.typed.ILookupImpl
      (get [self rect k] (.get self rect k nil))
      (get [_ rect k nf]
        (.valAt ^clojuer.lang.ILookup rect
          (case k
            (:l :left) :x
            (:t :top) :y
            :r :right
            :b :bottom
            k)
          nf))
      (getKeyCallSite [nf rect k]
        (if-some [fld (case k
                        (:x :l :left) '-_left
                        (:y :t :top) '-_top
                        (:r :right) '-_right
                        (:b :bottom) '-_bottom
                        :width 'getWidth
                        :height 'getHeight
                        nil)]
          (list '. rect fld)
          (if (and (= RRect cls)
                (= :radii k))
            (list :radii rect)
            nf))))))

(doit [cls [Point IPoint]]
  (util.typed/register-lookup-type! cls
    (reify chic.util.typed.ILookupImpl
      (get [self point k] (.get self point k nil))
      (get [_ point k nf]
        (.valAt ^clojure.lang.ILookup point
          (case k 
            (:l :t :left :top) :x
            (:r :b :right :bottom) :y 
            k)
          nf))
      (getKeyCallSite [nf point k]
        (if-some [fld (case k
                      (:x :width :left :l :t) '-_x
                        (:y :height :right :bottom :r :b) '-_y
                      nil)]
          (list '. point fld)
          nf)))))

; (defmacro x [form])
; (defmacro y [form])
; (defmacro r [form])
; (defmacro b [form])
