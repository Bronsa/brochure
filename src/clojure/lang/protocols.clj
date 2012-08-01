(ns clojure.lang.protocols)

(defprotocol IMutableValue
  (set-value! [this val])
  (get-value [this]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o new-meta]))

(defprotocol IResetMeta
  (-reset-meta! [o new-meta]))
