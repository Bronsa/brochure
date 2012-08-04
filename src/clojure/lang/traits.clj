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
