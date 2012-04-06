(ns newclojure.lang
  (:require [newclojure.lang.protocols :refer :all]
            [newclojure.lang.implementations :refer :all])
  (:import java.util.concurrent.atomic.AtomicReference))

(def ARef (merge IMetaImpl IWithMetaImpl IWatchableImpl))

(deftype Atom [^AtomicReference state
               ^:unsynchronized-mutable meta
               ^:volatile-mutable validator
               ^:volatile-mutable watches]

  :mixin (merge ARef IEquivImpl IHashImpl)

  IDeref
  (-deref [_] (.get state))

  IPrintable
  (-pr-seq [this opts]
    (concat ["#<Atom: "] (-pr-seq state opts) [">"]))

  IAtomicallyMutable
  (-reset! [this new-value]
    (-validate this new-value)
    (let [old-value (-deref this)]
      (.set state new-value)
      (-notify-watches this old-value new-value)
      new-value))
  (-compare-and-set! [this old-value new-value]
    (-validate this new-value)
    (when (.compareAndSet state old-value new-value)
      (-notify-watches this old-value new-value)
      true)))

