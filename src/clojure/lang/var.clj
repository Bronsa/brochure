(set! *warn-on-reflection* true)

(ns clojure.lang.var
  (:refer-clojure :exclude [deftype])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable AFn gen-invoke]]
            [brochure.def :refer [deftype]])
  (:import (java.util.concurrent.atomic.AtomicBoolean)))

(deftype ThreadBox [^:volatile-mutable value thread]
  IMutableField
  (set-field! [this new-val] (set! value new-val))
  (get-field [this] value))

(defn throw-arity [this n]
  (throw (IllegalStateException. (str "Attempting to call unbound fn:" (.var this)))))

(def UnboundFn
  (list 'IFn
    (gen-invoke `throw-arity)))

(deftype UnboundVar [var]

  :defaults [AFn UnboundFn]

  Object
  (toString [this] (str "Unbound: " var)))

;; (deftype Var [^:unsynchronized-mutable meta
;;               ^:volatile-mutable validator
;;               ^:volatile-mutable watches]

;;   :defaults [AReference AWatchable AValidable]

;;   IDeref
;;   (-deref [_]))
