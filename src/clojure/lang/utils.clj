(set! *warn-on-reflection* true)

(ns clojure.lang.utils
  (:refer-clojure :exclude [hash-combine])
  (:import java.util.Map$Entry
           java.util.concurrent.ConcurrentHashMap
           (java.lang.ref Reference ReferenceQueue)))

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

(defn clear-cache [^ReferenceQueue rq ^ConcurrentHashMap cache]
  (when-not (nil? (.poll rq))
    (loop [] (if-not (nil? (.poll rq)) (recur)))
    (doseq [^Map$Entry e (.entrySet cache)]
      (let [^Reference val (.getValue e)]
        (if (and (not (nil? val ))
                 (nil? (.get val)))
          (.remove cache (.getKey e) val))))))
