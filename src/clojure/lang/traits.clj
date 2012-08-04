(ns clojure.lang.traits
  (:require [brochure.def :refer [deftrait]]
            [clojure.lang.protocols :refer :all]))

(deftrait AReference [^:unsynchronized-mutable meta]
  IMeta
  (-meta [this]
    (locking this meta))
  
  IResetMeta
  (-reset-meta! [this m]
    (locking this (set! meta m))))

(deftrait AWatchable [^:volatile-mutable watches]
  IWatchable
  (-notify-watches [this old-value new-value]
    (doseq [[key f] watches]
      (f key this old-value new-value)))
  (-add-watch [this key f]
    (locking this
      (set! watches (assoc watches key f))))
  (-remove-watch [this key]
    (locking this
      (set! watches (dissoc watches key)))))

(deftrait AValidable [^:volatile-mutable validator]
  IValidable
  (-set-validator! [this new-validator]
    (when (new-validator (-deref this))
      (set! validator new-validator)))
  (-get-validator [_] validator)
  (-validate [_ new-value]
    (when validator
      (assert (validator new-value) "Validator rejected reference state"))))
