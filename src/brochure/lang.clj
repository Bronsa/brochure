(ns brochure.lang
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all])
  (:import java.util.concurrent.atomic.AtomicReference))

(deftype Atom [^AtomicReference state
               ^:unsynchronized-mutable meta
               ^:volatile-mutable validator
               ^:volatile-mutable watches]

  I-IMeta-mutable
  I-IWithMeta-mutable
  I-IWatchable
  I-IEquiv
  I-IHash
  
  IDeref
  (-deref [_] (.get state))

  IPrintable
  (-pr-seq [this opts]
    (concat ["#<Atom: "] (-pr-seq state opts) [">"]))

  IResettable
  (-reset! [this new-value]
    (-validate this new-value)
    (let [old-value (-deref this)]
      (.set state new-value)
      (-notify-watches this old-value new-value)
      new-value))

  IAtomicallyMutable
  (-compare-and-set! [this old-value new-value]
    (-validate this new-value)
    (when (.compareAndSet state old-value new-value)
      (-notify-watches this old-value new-value)
      true)))

