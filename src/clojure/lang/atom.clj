(set! *warn-on-reflection* true)

(ns clojure.lang.atom
  (:refer-clojure :exclude [deftype atom swap!])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable]]
            [brochure.def :refer [deftype]])
  (:import java.util.concurrent.atomic.AtomicReference))

(deftype Atom [^AtomicReference state
               ^:unsynchronized-mutable meta
               ^:volatile-mutable validator
               ^:volatile-mutable watches]

  :defaults [AReference AWatchable AValidable]

  IDeref
  (-deref [_] (.get state))

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

(defn atom
  ([x] (->Atom (AtomicReference. x) nil nil nil))
  ([x & {:keys [meta validator]}] (->Atom (AtomicReference. x) meta validator nil)))

(defn swap!
  [a f & args]
  (let [old-value (-deref a)
        new-value (apply f old-value args)]
    (and (-compare-and-set! a old-value new-value)
         new-value)))
