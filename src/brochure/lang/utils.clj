(ns brochure.lang.utils
 (:require [brochure.lang.protocols :refer :all]))

(extend-type nil
  ISeqable
  (-seq [_] nil))

(defn rest- [coll]
  (require 'brochure.lang.PersistentList)
  (or (-next coll)
      ((resolve 'brochure.lang.PersistentList/->EmptyList) {}))) ;; HERP DERP

(defn count- [coll]
  (cond
    (nil? coll) 0

    (satisfies? ICounted coll) (-count coll)

    (satisfies? IPersistent coll)
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

    (-> coll .getClass .isArray)
      (java.lang.reflect.Array/getLength coll)

    :else
      (throw (UnsupportedOperationException.
              (str "count not supported on this type: "
                   (-> coll .getClass .getSimpleName))))))

(defn cons-
  [x seq]
  (require 'brochure.lang.PersistentList) ;; herping the derp
  ((resolve 'brochure.lang.PersistentList/->Cons) x (-seq seq) nil))

(defmacro doseq- [[n f] & body]
  `(loop [s# (-seq ~f) ~n (-first s#)]
     (if (-seq s#)
       (do ~@body
           (recur (rest- s#) (-first (next- s#)))))))

(defn reduce-
  ([f coll]
     (reduce- f (-first coll) (rest- coll)))
  ([f init coll]
     (loop [v init s (-first coll) r (-next coll)]
       (if r
         (recur (f v s) (-first r) (-next r))
         (f v s)))))

(defn every?- [pred coll]
  (cond
    (nil? (-seq coll)) true
    (pred (-first coll)) (recur pred (-next coll))
    :else false))

(defn map-
  ([f coll]
     (when-let [s (-seq coll)]
       (cons- (f (-first s))
              (map- f (rest- s)))))
  ([f coll & colls]
     (letfn [(step [cs]
               (let [ss (map seq cs)]
                 (when (every? identity ss)
                   (cons- (map first ss) (step (map rest ss))))))]
       (map #(apply f %) (step (conj colls coll))))))

(defn hash- [obj]
  (if obj
    (if (satisfies? IHash obj)
      (-hash obj)
      (.hashCode obj))
    0))

(def seqable-hash-code
  (memoize
   (fn [obj]
     (reduce- #(+ (* 31 %)
                  (if %2 (.hashCode %2) 0)) 1 obj))))

(defn reify-seq [seq]
  (java.util.Collections/unmodifiableList (java.util.ArrayList. seq)))

(defn equals? [a b]
  (or (identical? a b)
      (and (not (nil? a))
           (.equals a b))))

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

