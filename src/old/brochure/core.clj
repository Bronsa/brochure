(ns brochure.core
  (:refer-clojure :exclude [atom swap! meta with-meta force])
  (:require [brochure.lang :refer :all]
            [brochure.lang.protocols :refer :all])
  (:import java.util.concurrent.atomic.AtomicReference))

(defn meta [x]
  (-meta x))

(defn with-meta [x meta]
  (-with-meta x meta))

(defn force [obj]
  (if (satisfies? IPending obj) (-deref obj) obj))

;;etc etc

(defn atom
  ([x] (->Atom (AtomicReference. x) nil nil nil))
  ([x & {:keys [meta validator]}] (->Atom (AtomicReference. x) meta validator nil)))

(defn swap!
  [a f & args]
  (let [old-value (-deref a)
        new-value (apply f old-value args)]
    (and (-compare-and-set! a old-value new-value)
         new-value)))