(ns jl.sm.dev)

"Concise API for programming JVM assembly. In spirit of Factor."




astore
monitorenter
A
aload
monitorexit
B
return
aload ; handle
monitorexit
athrow
'
(def compute-max-saturation*
  [:a :b :k0 :k1 :k2 :k3 :k4 :wl :wm :ws]
  (l :a
    (l :b (*))
    (l :k4 (*))
    (l :a
      (dup (*))
      (l :k3 (*))
      (+))
    (l :b
      (l :k2 (*))
      (+))
    (l :a
      (l :k1 (*))
      (+))
    (l :k0 (+))
    (s+ :S))
)

(defn compute-max-saturation*
  [^Class F ^F a ^F b
   ^F k0 ^F k1 ^F k2 ^F k3
   ^F k4 ^F wl ^F wm ^F ws]
  (let [S (+ k0
            (* a k1)
            (* b k2)
            (* a a k3)
            (* a b k4))
        k-l (+ (* a 0.3963377774) (* b 0.2158037573))
        l' (+ 1 (* S k-l))
        l (* l' l' l')
        l-ds (* 3 k-l l' l')
        l-ds2 (* 6 k-l k-l l')
        
        k-m (+ (* a -0.1055613458) (* b -0.0638541728))
        m' (+ 1 (* S k-m))
        m (* m' m' m')
        m-ds (* 3 k-m m' m')
        m-ds2 (* 6 k-m k-m m')
        
        k-s (+ (* a -0.0894841775) (* b -1.2914855480))
        s' (+ 1 (* S k-s))
        s (* s' s' s')
        s-ds (* 3 k-s s' s')
        s-ds2 (* 6 k-s k-s s')
        
        f (+ (* wl l) (* wm m) (* ws s))
        f1 (+ (* wl l-ds) (* wm m-ds) (* ws s-ds))
        f2 (+ (* wl l-ds2) (* wm m-ds2) (* ws s-ds2))]
    (- S (* f (/ f1
                (- (* f1 f1)
                  (* 0.5 f f2)))))))