(set! *warn-on-reflection* true)

(ns clojure.lang.utils
  (:refer-clojure :exclude [hash-combine hash count])
  (:require [clojure.lang.protocols :refer :all])
  (:import java.util.Map$Entry
           java.util.concurrent.ConcurrentHashMap
           (java.lang.ref Reference ReferenceQueue)))

(defn hash-code [^Object o]
  (if (nil? o) 0
      (.hashCode o)))

(defn hash [o]
  (if (satisfies? IHash o)
    (-hash o)
    (hash-code o)))

(set! *unchecked-math* true)
(defn hash-combine [seed hash]
  (let [seed (int seed)
        hash (int hash)]
    (bit-xor seed
             (unchecked-add-int hash
               (unchecked-add-int 0x9e3779b9
                 (unchecked-add-int
                  (clojure.lang.Numbers/shiftLeftInt seed 6)
                  (clojure.lang.Numbers/shiftRightInt seed 2)))))))

(def seqable-hash-code
  (memoize
   (fn [obj]
     (int (reduce #(unchecked-add-int (int (* 31 %))
                                      (int (hash %2))) 1 obj)))))
(set! *unchecked-math* false)

(defn clear-cache [^ReferenceQueue rq ^ConcurrentHashMap cache]
  (when-not (nil? (.poll rq))
    (loop [] (if-not (nil? (.poll rq)) (recur)))
    (doseq [^Map$Entry e (.entrySet cache)]
      (let [^Reference val (.getValue e)]
        (if (and (not (nil? val ))
                 (nil? (.get val)))
          (.remove cache (.getKey e) val))))))

(defn equals? [a b]
  (or (identical? a b)
      (and (not (nil? a))
           (.equals ^Object a b))))

(defn equiv? [a b]
  (or (identical? a b)
      (and a
           (if (and (instance? java.lang.Number a)
                    (instance? java.lang.Number b))
             (clojure.lang.Numbers/equal a b)
             (if (satisfies? IPersistentCollection a)
               (-equiv a b)
               (if (satisfies? IPersistentCollection b)
                 (-equiv b a)
                 (.equals ^Object a b)))))))


(deftype Box [val])

(defprotocol IState
  (-val [this])
  (-val! [this v])
  (-rest [this])
  (-rest! [this v]))

(deftype State [^:volatile-mutable val ^:volatile-mutable rest]
  IState
  (-val [_] val)
  (-val! [_ new-val] (set! val new-val))
  (-rest [_] rest)
  (-rest! [_ new-rest] (set! rest new-rest)))


(defn count [coll]
  (cond
    (nil? coll) 0

    (satisfies? ICounted coll) (-count coll)

    (satisfies? IPersistentCollection coll)
    (loop [s (-seq coll) c 0]
      (if (-next s)
        (if (satisfies? ICounted s)
          (+ c (-count s))
          (recur (-next s) (inc c)))
        c))
      
    (instance? CharSequence coll)
    (.length ^CharSequence coll)

    (instance? java.util.Collection coll)
    (.size ^java.util.Collection coll)
      
    (instance? java.util.Map coll)
    (.size ^java.util.Map coll)
      
    (-> ^Object coll .getClass .isArray)
    (alength ^objects coll)
    
    :else
    (throw (UnsupportedOperationException.
            (str "count not supported on this type: "
                 (-> ^Object coll .getClass .getSimpleName))))))


(defn reify-seq ^java.util.List [^java.util.List seq]
  (java.util.Collections/unmodifiableList (java.util.ArrayList. seq)))

(defn unsigned-bit-shift-right
  "Shifts the input `x` to the right by `n` places and sets the leftmost bit to 0."
  [^long x ^long n]
  (bit-and 0xefffffff (bit-shift-right x n)))

(defn copy-array
  "Copy the elements in `from-array` to `to-array`.  Assumes that
   `to-array` is as long as `from-array`."
  [^objects from-array ^objects to-array]
  (loop [c (count from-array)]
    (when (> c 0)
      (aset to-array (dec c) (aget from-array (dec c)))
      (recur (dec c))))
  to-array)

(defn seq-to-array
  ([this]
     (let [o (object-array (count this))]
       (loop [curr (-seq this) i 0]
         (when curr
           (aset o i (-first curr))
           (recur (-next curr) (inc i))))
       o))
  ([this ^objects a]
     (let [len (count this)
           ^objects o (if (> len (alength a))
                        (java.lang.reflect.Array/newInstance (-> a .getClass .getComponentType) ^int len)
                        a)]
       (loop [curr (-seq this) i 0]
         (when curr
           (aset o i (-first curr))
           (recur (-next curr) (inc i))))
       (when (< len (alength a))
         (aset o len nil))
       o)))
