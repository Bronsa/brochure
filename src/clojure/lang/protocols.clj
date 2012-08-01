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
