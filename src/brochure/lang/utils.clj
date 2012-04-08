(ns brochure.lang.utils
 (:require [brochure.lang.protocols :refer :all]))

(extend-type nil
  ISeqable
  (-seq [_] nil))

(defn next- [coll]
  (-seq (-rest coll)))

(defn cons-
  [x seq]
  (require 'brochure.lang.PersistentList) ;; herping the derp
  ((resolve 'brochure.lang.PersistentList/->Cons) x (-seq seq) nil))

(defmacro doseq- [[n f] & body]
  `(loop [s# (-seq ~f) ~n (-first s#)]
     (if (-seq s#)
       (do ~@body
           (recur (-rest s#) (-first (-rest s#)))))))

(defn reduce-
  ([f coll]
     (reduce- f (-first coll) (-rest coll)))
  ([f init coll]
     (loop [v init s (-first coll) r (next- coll)]
       (if r
         (recur (f v s) (-first r) (next- r))
         (f v s)))))

(defn every?- [pred coll]
  (cond
    (nil? (-seq coll)) true
    (pred (-first coll)) (recur pred (next- coll))
    :else false))

(defn map-
  ([f coll]
     (when-let [s (-seq coll)]
       (cons- (f (-first s))
              (map- f (-rest s)))))
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

