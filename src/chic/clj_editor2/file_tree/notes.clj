(ns chic.clj-editor2.file-tree.notes)

;; identify temp bindings that are not used in the draw function - make them local, not fields

;; ideas:
;; fade out underscore when folder not expanded

;; watching file tree:
"
on modify:
  maybe update single file name
on delete/create:
  if fully below top: shift entries below
  if above that: shift entries above
on overflow: rebuild

lazy loading & watching:
  always keep in memory: visible items, all parents, (some) offscreen siblings of top & bottom.
  when scrolling, check if new items are expanded
    if so, get first/last child and recur
  chunking.
"

;; idea: initConstants method for component so consumer can pass inputs that do not change
;; so the component can initialise fields that depend only on the constant inputs

;; erase constant fields in field depmasks

(comment
  (deftype ____ [input-provider ;; field or argument?
                 ^int change-mask ;; ^
                 ^float baseline-y ^float text-x ^Image icon-image icon-image-rect]
    (draw [_ ^Canvas cnv]
      '...)
    (notify-changed [_ ks] ;; only if input-provider is field
      ;; notify when inputs changed
      ;; walk dep graph and mark changed properties
      '...)
    (recalculate [_ input-provider change-mask])
    (recalculate-all [_ input-provider])
    ;; or
    (recalc2 [_ centre-y x height textblob ...etc]))
  ;; or just use identical? diffing

  ;; separate objects for active values?
  ;; method to pass prev&new values and return a change mask - useful perhaps for repeated components
  ;; pass each input as a separate argument to avoid additional method calls.
  ;; One param could be reserved for the root context (eg mouse pos, jwm window)

  ;; There are inputs and there are dependent values (latter are functions of input and do not change directly).
  ;; But you may want mutable properties that can be directly set.

  ;; Could have active values that have a reference to a list of their dependents.
  ;; When active value is directly modified, all dependents are walked and their change field incremented
  ;; The directly modified values are stored as entry points for recalculation.
  ;; The next frame, all with change>=1 are recalculated, but ordered by the change field (1s then 2s etc)
  ;; Maybe this could help the case where two independent active values share the same dependents
  ;; But is complicated
  ;; Could also have an option to directly recalculate on modifying a value.
  ;; Probably should start with a more functional tree model.

  (deftype ____ [^float baseline-y ^float text-x ^Image icon-image icon-image-rect]
    (draw [_ inputs... ^Canvas cnv]
      '...)
    (recalculate [_ inputs... change-mask]))
  #!
  )

(defn bcoll-search-insertion-idx
  ^long [^java.util.Comparator cmptor ^io.lacuna.bifurcan.ICollection coll k]
  (loop [min-idx 0
         max-idx (.size coll)]
    (if (== min-idx max-idx)
      max-idx
      (let [idx (unchecked-add
                 min-idx (unchecked-int
                          (Math/floor (unchecked-multiply
                                       util/phi-1 (unchecked-subtract max-idx min-idx)))))
            item (.nth coll idx)]
        (if (< 0 (.compare cmptor item k))
          (recur min-idx idx)
          (recur (unchecked-inc idx) max-idx))))))

(comment
  (let [lst (List/from [0 1 2 3 4 6 6 6 6 9 10])]
    (and (= 9 (bcoll-search-insertion-idx compare lst 6))
         (= 4 (bcoll-search-insertion-idx compare lst 3))
         (= 0 (bcoll-search-insertion-idx compare lst -1))
         (= 11 (bcoll-search-insertion-idx compare lst 11))))

;; use chunking for subview, but only draw visible
  ;; for now, all file items in memory
  (def --comparator)
  (definterface IReplClass
    (reset [paths])
    (create [^java.nio.file.Path path])
    (modify [^java.nio.file.Path path])
    (delete [^java.nio.file.Path path]))
  (deftype ReplClass [^:unsynchronized-mutable ^List file-names
                      ^:unsynchronized-mutable ^List cmpts
                      ^int cmpt-offset]
    clojure.lang.ILookup
    (valAt [self k] (.valAt self k nil))
    (valAt [_ k nf]
      (case k
        :file-names file-names
        :cmpts cmpts
        nf))

    IReplClass
    (reset [_ paths]
      (let [file-names-tmp (.linear List/EMPTY)
            a (to-array paths)]
        (java.util.Arrays/sort a)
        (doary [p a]
          (.addLast file-names-tmp (fs/file-name p)))
        (set! file-names (.forked file-names-tmp))))

    (create [_ path]
      (let [fname (fs/file-name path)
            insert-idx (bcoll-search-insertion-idx
                        (java.util.Comparator/naturalOrder) file-names fname)]
        (set! file-names
              (-> (.slice file-names 0 insert-idx)
                  (.addLast fname)
                  (.concat (.slice file-names insert-idx (.size file-names)))))))

    (modify [_ path])

    (delete [_ path]
      (let [fname (fs/file-name path)
            idx (bcoll-search-insertion-idx
                 (java.util.Comparator/naturalOrder) file-names fname)
            size (.size file-names)]
        (set! file-names
              (-> (.slice file-names 0 idx)
                  (cond-> (< idx size)
                    (.concat (.slice file-names (unchecked-inc idx) size))))))))

  (def --thing (->ReplClass List/EMPTY List/EMPTY 0))
  (.create --thing (nth (fs/list-dir ".") 0))
  (.reset --thing (take 3 (fs/list-dir ".")))
  (doseq [p (drop 3 (fs/list-dir "."))]
    (.create --thing p))

#!
  )


  ;; TODO need the tree to calculate size then later receive position
  ;; separate recalculate function? - similar params as draw function
  ;;   - additional position params for draw function
  ;; "prepare" or measure function?
  ;; benefit of dedicated measure function is potentially fewer args than draw function
  ;;   but dividing up the field calculations seems wrong

  ;; direct components can still exist

  ;; issue where parent may invalidate calculations of child:
  "child calcs visible elements based on position.
but parent can change position.
Extra tricky if size eg width depends on the visible elements.
   - but this is a special case where behaviour has to be carefully considered anyway
So it may make sense to have a two-phase component with a clear distinction
between pre-and-post position data availability

What if it were implemented like a callback eg
..size calcs
rect (measure-self) ;; finishes off parent calcs and returns size
..rest of calcs

This feels complicated to implement, but is a nice way to express the divide.
More generally, this can be 'deferred inputs' - depends on both inputs
and fields

How would this look on the parent side? Perhaps something like blocking.
And then what about multiple children?

What about different size constraints? infinite vs fixed.
If fixed, the component should not calculate size at all.
Component may depend on size for its content, but if no size restrictions
then content determines size (reverse).
Note: stretch and shrink are very different behaviours.

Constraints useful here? max-size, min-size, stretch, shrink
Constraints on rect: max-boundary, min-boundary, max-size, min-size
Align: stretch, shrink + horiz/vertical
Arrange: relative to other things
(instead of returning size, the absolute rect is calcd from constraints)

If position can be expressed in terms on constraints on size, then
you get the benefit of declarative code and less freaky control flow.
Simple constraint on position is probably enough for simple scroll rect view.
TODO do this.
"
