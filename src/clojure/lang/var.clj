(set! *warn-on-reflection* true)

(ns clojure.lang.var
  (:refer-clojure :exclude [deftype atom swap!])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable AFn AVMutable gen-invoke]]
            [clojure.lang.atom :refer [atom swap!]]
            [brochure.def :refer [deftype]])
  (:import (java.util.concurrent.atomic.AtomicBoolean)))

(deftype ThreadBox [^:volatile-mutable value thread]
  :defaults [AVMutable])

(declare throw-arity)

(def UnboundFn
  (cons []
    (gen-invoke `throw-arity)))

(deftype UnboundVar [var]

  :defaults [AFn UnboundFn]

  Object
  (toString [this] (str "Unbound: " var)))

(deftype Frame [bindings prev])

(def rev (atom 0))

(defn make-frame
  ([] (Frame. {} nil))
  ([bindings] (Frame. bindings nil))
  ([bindings prev] (Frame. bindings prev)))

(defn clone-frame [^Frame frame]
  (make-frame (.bindings frame)))

(def dvals (proxy [ThreadLocal] []
               (initalValue [] (make-frame))))

(defn throw-arity [^UnboundVar this n]
  (throw (IllegalStateException. (str "Attempting to call unbound fn:" (.var this)))))

;; (deftype Var [^:unsynchronized-mutable meta
;;               ^:volatile-mutable validator
;;               ^:volatile-mutable watches]

;;   :defaults [AReference AWatchable AValidable]

;;   IDeref
;;   (-deref [_]))
