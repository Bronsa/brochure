(ns brochure.lang.PersistentVector
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang.utils :refer :all]))

;; (deftype PersistentVector []

;;   ;; ISeqable
;;   ;; (-seq [this]
;;   ;;   (if (pos? (-count this))
;;   ;;     (->Seq this 0)))
  
;;   IFn
;;   (-invoke [this idx]
;;     (anssert (integer? idx) "Key Must be integer")
;;     (-nth this (.intValue idx))))


(deftype VectorNode [^objects array])

(def empty-node (VectorNode. (object-array 32)))

(defn- array-for
  "A helper function that finds the array containing element `i` "
  [vec i]
  (let [cnt (.cnt vec)
        ^VectorNode root (.root vec)
        shift (.shift vec)
        ^objects tail (.tail vec)
        tailoff (if (< cnt 32) 0 (bit-shift-left (unsigned-bit-shift-right (dec cnt) 5) 5))]
     (if (and (>= i 0) (< i cnt))
       (if (>= i tailoff)
         tail
         (loop [^VectorNode node root level shift]
           (let [^objects arr (-array node)]
             (if (<= level 0)
               arr
               (let [new-node (aget arr (bit-and (unsigned-bit-shift-right i level) 0x01f))]
                 (recur new-node (- level 5)))))))
       (throw (IndexOutOfBoundsException.)))))

(deftype PVector [cnt shift ^VectorNode root ^objects tail _meta]
  IWithMeta
  (-with-meta [this m]
    (PVector. cnt shift root tail m))

  IMeta
  (-meta [this] _meta)

  
;  clojure.lang.IPersistentVector
;  (length [this] cnt)

  ;; Change the `i`th value in the vector to `val`.

  IVector
  (-assoc-n [this i val]
    (let [tailoff (if (< cnt 32) 0 (bit-shift-left (unsigned-bit-shift-right (dec cnt) 5) 5))]
     (cond (and (>= i 0) (< i cnt))
           (if (>= i tailoff)
             (let [^objects new-tail (empty-array (count tail))
                   _ (copy-array tail new-tail)
                   _ (aset new-tail (bit-and i 0x1f) val)]
               (PVector. cnt shift root new-tail _meta))
             (let [do-assoc (fn do-assoc [level node i val]
                              (let [node-array (-array node)
                                    new-array (empty-array (count node-array))
                                    _ (copy-array node-array new-array)
                                    new-node (VectorNode. new-array)]
                                (if (= level 0)
                                  (do (aset new-array (bit-and i 0x01f) val)
                                      new-node)
                                  (let [subidx (bit-and (unsigned-bit-shift-right i level) 0x01f)]
                                    (aset new-array subidx (do-assoc (- level 5) (aget node-array subidx) i val))
                                    new-node))))]
               (PVector. cnt shift (do-assoc shift root i val) tail _meta)))
           (= i cnt)
           (-conj this val)
           :else
           (throw (IndexOutOfBoundsException.)))))

  ;; Add an element to the end of the vector.

  ICollection
  (-conj [this o]
    (let [tailoff (if (< cnt 32) 0 (bit-shift-left (unsigned-bit-shift-right (dec cnt) 5) 5))]
      (if (< (- cnt tailoff) 32)
       (let [tail-count (count tail)
             ^objects new-tail (empty-array (inc tail-count))
             _ (copy-array tail new-tail)
             _ (aset new-tail tail-count o)]
         (PVector. (inc cnt) shift root new-tail _meta))
       (let [tail-node (VectorNode. tail)
             overflow-root? (> (unsigned-bit-shift-right cnt 5) (bit-shift-left 1 shift))
             new-path (fn new-path [level node]
                        (if (= level 0)
                          node
                          (let [^objects new-array (empty-array 32)
                                ret (VectorNode. new-array)
                                _ (aset new-array 0 (new-path (- level 5) node))]
                            ret)))
             push-tail (fn push-tail [level parent tail-node]
                         (let [subidx (bit-and (unsigned-bit-shift-right (dec cnt) level) 0x01f)
                               ^objects parent-array (-array parent)
                               ^objects new-arr (empty-array (count parent-array))
                               _ (copy-array parent-array new-arr)
                               ret (VectorNode. new-arr)
                               node-to-insert (if (= level 5)
                                                tail-node
                                                (let [child (aget parent-array subidx)]
                                                  (if child
                                                    (push-tail (- level 5) child tail-node)
                                                    (new-path (- level 5) tail-node))))
                               _ (aset new-arr subidx node-to-insert)]
                           ret))

             [new-shift new-root]
             (if overflow-root?
               (let [^objects new-root-array (empty-array 32)
                     _ (aset new-root-array 0 root)
                     _ (aset new-root-array 1 (new-path shift tail-node))]
                 [(+ shift 5) (VectorNode. new-root-array)])
               [shift (push-tail shift root tail-node)])]
         (PVector. (inc cnt) new-shift new-root (to-array (list o)) _meta)))))
  
  IEmptyableCollection
  (-empty [this]
    (PVector. 0 5 empty-node (to-array '()) _meta))

  ;; Check whether another sequence has all the same elements as this
  ;; vector.
  IEquiv
  (-equiv [this o]
    (if (or (list? o) (vector? o))
      (if (not= (count o) (-count this))
        false
        (every? (map = o (-seq this))))
      (if (not (sequential? o))
        false
        (loop [s (-seq this) a (seq o)]
          (cond (and (nil? s) (nil? a))
                true
                (nil? s)
                false
                (nil? a)
                false
                (not= (first s) (first a))
                false
                :else
                (recur (rest s) (rest a)))))))

  IAssociative

  #_(-assoc [this key val]
    (if (integer? key)
      (.assocN this key val)
      (throw (IllegalArgumentException. "Key must be integer"))))

  #_(entryAt [this key]
    (when (and (integer? key) (>= key 0) (< key cnt))
      (MapEntry. key (-nth this key))))

  (-contains-key [this key]
    (and (integer? key) (>= key 0) (< key cnt)))
  
  IStack

  ;; Return the last element of the vector
  
  (-peek [this]
    (if (> (-count this) 0)
      (-nth this (dec cnt))))

  ;; Return the vector without its last element.
  ;; Not yet implemented.

  (-pop [this]
    (throw (UnsupportedOperationException.)))
  
  ISeqable
  (-seq [this]
    (ChunkedVector. this (array-for this 0) 0 0 {}))
  
  IFn
  (-invoke [this k] (-nth this k))
  (-invoke [this k not-found] (-nth this k not-found))
  
  IIndexed
  (-nth [this i]
    (let [node (array-for this i)]
      (aget node (bit-and i 0x01f))))
  (-nth [this i not-found]
    (if (and (>= i 0) (< i cnt))
      not-found
      (-nth this i)))

  IHash
  (-hash [this]
    (reduce #(+ (* 31 %) (hash %2)) (-seq this)))
  
  ICounted
  (-count [this]
    cnt))

;; ### Creating the persistent vector

;; The naive way of constructing a persistent vector from a collection
;; is to repeatedly `conj` elements of the collection onto the empty
;; vector.  Though this works, it is slow for a couple of reasons.
;; First, it creates a lot of extra objects to be GC'd without a lot
;; of purpose.  Second, it takes O(n log n) time (where the base of
;; the log is 32, so the log n factor will always be pretty small, but
;; still) rather than the O(n) time that is both optimal and possible.

;; The algorithm works from the bottom up by repeatedly partitioning
;; the input into groups of 32.  Since the size of the input at each
;; step is a constant fraction of the previous size, and a linear
;; amount of work is done at each step, this is a linear-time
;; algorithm.

(defn ^:export pvec
  "Construct a PVector from the collection `coll` in linear time."
  [coll]
  (let [reversed-partition (fn reversed-partition [n coll]
                             (loop [ret () co coll remaining (count coll)]
                               (if (= 0 remaining)
                                 ret
                                 (let [rem (min 32 remaining)
                                       ^objects ret-array (empty-array rem)
                                       next-co (loop [cnt 0 coll co]
                                                 (if (= cnt rem)
                                                   coll
                                                   (do (aset ret-array cnt (first coll))
                                                       (recur (inc cnt) (rest coll)))))]
                                  (recur (cons ret-array ret) next-co (- remaining rem))))))
        grouped-coll (reversed-partition 32 coll)
        big-groups (rest grouped-coll)
        tail (first grouped-coll)
        reversed-map (fn [f coll]
                       (loop [ret () coll coll]
                         (if (empty? coll)
                           ret
                           (recur (cons (f (first coll)) ret) (rest coll)))))
        c (+ (* 32 (dec (count grouped-coll))) (count tail))
        shift (loop [level 0 c (unsigned-bit-shift-right c 5)]
                 (if (<= c 32)
                   (* 5 (inc level))
                   (recur (inc level) (bit-shift-right c 5))))
        root (loop [groups big-groups level shift]
               (let [vector-nodes (reversed-map #(VectorNode. %) groups)]
                 (if (= level 5)
                   (VectorNode. (to-array vector-nodes))
                   (recur (reversed-partition 32 vector-nodes) (- level 5)))))]
    (PVector. c shift root tail {})))

(defn empty-pvector
  "Create an empty PVector"
  []
  (PVector. 0 5 empty-node (to-array '()) {}))

