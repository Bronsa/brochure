(ns clojure.lang.delay
  (:require [clojure.lang.protocols :refer :all]))

(deftype Delay [^:unsynchronized-mutable val ^:unsynchronized-mutable f]

  IDeref
  (-deref [this]
    (when f
      (locking this
        (when f
          (set! val (f))
          (set! f nil))))
    val)

  IPending
  (-realized? [this]
    (locking this
      (nil? f))))
