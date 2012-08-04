(ns clojure.lang.protocols)

(defprotocol IMutableField
  (set-field! [this val])
  (get-field [this]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o new-meta]))

(defprotocol IResetMeta
  (-reset-meta! [o new-meta]))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol IValidable
  (-set-validator! [this validator])
  (-get-validator [this])
  (-validate [this args]))

(defprotocol ISettable
  (-set! [this new-value]))

(defprotocol IDeref
  (-deref [o]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(defprotocol IAtomicallyMutable
  (-compare-and-set! [this old-value new-value])
  (-reset! [this new-value]))

(defprotocol IPending
  (-realized? [d]))
