(ns newclojure.lang.implementations)

;;mutable impl
(def IMetaImpl
  '{IMeta
    [(^:synchronized -meta [_] meta)]})
;;mutable impl
(def IWithMetaImpl
  '{IWithMeta
    [(^:synchronized -with-meta [_ new-meta] (set! meta new-meta))]})

(def IWatchableImpl
  '{IWatchable
    [(-notify-watches [this old-value new-value]
       (doseq [[key f] watches]
         (f key this old-value new-value)))
     (^:synchronized -add-watch [_ key f]
       (set! watches (assoc watches key f)))
     (^:synchronized -remove-watch [_ key]
       (set! watches (dissoc watches key)))]})

(def IValidableImpl
  '{IValidable
    [(-set-validator! [this new-validator]
       (when (new-validator (-deref this))
         (set! validator new-validator)))
     (-get-validator [_] validator)
     (-validate [_ new-value]
       (when validator
         (assert (validator new-value) "Validator rejected reference state")))]})

(def IEquivImpl
  '{IEquiv
    [(-equiv [this other] (identical? this other))]})

(def IHashImpl
  '{IHash
    [(-hash [this] (.hashCode this))]})
