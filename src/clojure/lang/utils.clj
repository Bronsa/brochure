(set! *warn-on-reflection* true)

(ns clojure.lang.utils
  (:refer-clojure :exclude [hash-combine]))

(defn hash-code [^Object o]
  (if (nil? o) 0
      (.hashCode o)))

(set! *unchecked-math* true)
(defn hash-combine [seed hash]
  (let [seed (int seed)
        hash (int hash)]
    (bit-xor seed
             (+ hash 0x9e3779b9
                (bit-shift-left seed 6) ;; should probably be using Numbers/shiftLeftInt
                (bit-shift-right seed 2)))))
