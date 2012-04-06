(ns newclojure.lang.PersistentList
  (:require [newclojure.lang.protocols :refer :all]))

(declare ->Cons)

(extend-type nil
  ISeqable
  (-seq [_] nil))

(defn cons-
  [x seq]
  (->Cons x (-seq seq) nil))

(defn next- [coll]
  (-seq (-rest coll)))

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

(deftype SeqIterator [^:unsynchronized-mutable seq]
  java.util.Iterator
  (hasNext [_] (boolean seq))
  (next [_]
    (if seq
      (let [ret (-first seq)]
        (set! seq (-seq (-rest seq)))
        ret)
      (throw (java.util.NoSuchElementException.))))
  (remove [_] (throw (java.util.NoSuchElementException.))))

(def ListImpl
  '{java.util.List
    [(contains [this o]
       (boolean (some #{o} this)))
     (containsAll [this c]
       (every? (set c) this))
     (get [this index] (nth this index)) ;; make sure nth impl doesnt depend on .get or WE'RE FUCKED
     (indexOf [this o]
       (loop [coll this i 0]
         (if (seq coll)
           (or (when (= o (first coll)) i)
               (recur (next coll) (inc i)))
           -1)))
     (isEmpty [this]
       (nil? (seq this)))
     (iterator [this]
       (SeqIterator. this))
     (lastIndexOf [this o]
       (.lastIndexOf (reify-seq this) o))
     (listIterator [this]
       (.listIterator (reify-seq this)))
     (listIterator [this index]
       (.listIterator (reify-seq this) index))
     (size [this] (-count this))
     (subList [this fromIndex toIndex]
       (.subList (reify-seq this) fromIndex toIndex))
     (toArray [this]
       (let [o (object-array (-count this))]
         (loop [curr (-seq this) i 0]
           (when curr
             (aset o i (-first curr))
             (recur (next- curr) (inc i))))
         o))
     (toArray [this a]
       (let [len (-count this)
             o (if (> len (.length a))
                 (java.lang.reflect.Array/newInstance (-> a .getClass .getComponentType) len)
                 a)]
         (loop [curr (-seq this) i 0]
           (when curr
             (aset o i (-first curr))
             (recur (next- curr) (inc i))))
         (when (< len (.lenght a))
           (aset o len nil))
         o))
     (add [_ o]
       (throw (UnsupportedOperationException.)))
     (add [_ index element]
       (throw (UnsupportedOperationException.)))
     (addAll [_ c]
       (throw (UnsupportedOperationException.)))
     (addAll [_ index c]
       (throw (UnsupportedOperationException.)))
     (clear [_]
       (throw (UnsupportedOperationException.)))
     (remove [_ ^int index]
       (throw (UnsupportedOperationException.)))
     (^boolean remove [_ o]
       (throw (UnsupportedOperationException.)))
     (removeAll [_ c]
       (throw (UnsupportedOperationException.)))
     (retainAll [_ c]
       (throw (UnsupportedOperationException.)))
     (set [this index element]
       (throw (UnsupportedOperationException.)))]})

(set! *unchecked-math* true) ;; we need this to overflow on -hash
(def ASeq
  (merge
   '{ISeq [(-first [_] first)
           (-rest [_] rest)]

     ISequential nil

     java.io.Serializable nil

     IHash [(-hash [this]
              (int
               (reduce- #(+ (* 31 %)
                            (hash- %2)) 1 this)))]
     
     IEmptyableCollection
     [(-empty [_] (->EmptyList))]

     ISeqable
     [(-seq [this] this)]

     ICollection
     [(-conj [this o]
        (PersistentList. o this (inc (-count this)) meta))]

     Object
     [(hashCode [this]
        (reduce- #(+ (* 31 %)
                     (if %2 (.hashCode %2) 0)) 1 this))]
     IMeta
     [(-meta [_] meta)]}
   ListImpl))

(declare ->EmptyList)

;; this should be unnecessary, will remove this once we fix it on clojure
(def ConsImpl
  '{ISeq
    [(-first [coll] first)
     (-rest [coll] (if (nil? rest) (->EmptyList {}) rest))]

    IEmptyableCollection

    [(-empty [_] (with-meta (->EmptyList) meta))]

    ICollection

    [(-conj [coll o] (Cons. nil o coll))]

    ICounted
    [(-count [_] (inc (-count rest)))]})

(deftype Cons [first rest meta]
  :mixin (merge ASeq ConsImpl)

  IWithMeta
  (-with-meta [_ new-meta] (Cons. meta first rest)))

(defn equals? [a b]
  (or (identical? a b)
      (and (not (nil? a))
           (.equals a b))))

;; (defn identical? [a b]) == java operator
(deftype PersistentList [first rest count meta] ;rest is never nil

  :mixin ASeq

  IPersistent
  IList

  IWithMeta
  (-with-meta [_ new-meta]
    (PersistentList. first rest new-meta count))

  IStack
  (-peek [_] first)
  (-pop [_] rest)

  ICounted
  (-count [_] count)

  Object
  (equals [this o]
    (or (identical? this o)
        (-equiv this o)))

  ;; (toString [this]
  ;;   (apply str (flatten (-pr-seq this nil))))

  ;; rip printing from clojurescript.
  ;; IPrintable
  ;; (-pr-seq [_ opts]
  ;;   (pr-sequential pr-seq "(" " " ")" opts coll))

  IEquiv
  (-equiv [this o]
    (and (or (isa? ISequential o) (isa? java.util.List o))
         (every? true? (map equals? this o)))))

(def ListImplEmpty
  '{java.util.List
    [(size [_] 0)
     (isEmpty [_] true)
     (contains [_ o] false)
     (iterator [_]
       (reify java.util.Iterator
         (hasNext [_] false)
         (next [_]
           (throw (java.util.NoSuchElementException.)))
         (remove [_]
           (throw (UnsupportedOperationException.)))))
     (toArray [_] (object-array 0))
     (toArray [_ o]
       (if (> (.length o) 0)
         (aset o 0 nil))
       o)]})

(deftype EmptyList [meta]

  :mixin (merge ListImpl ListImplEmpty)

  IPersistent
  IList
  ISequential

  IWithMeta
  (-with-meta [this new-meta]
    (if (not= new-meta meta)
      (EmptyList. new-meta)
      this))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-rest [coll] nil)

  IStack
  (-peek [coll] nil)
  (-pop [coll] (throw (Exception. "Can't pop empty list")))

  ICollection
  (-conj [coll o] (PersistentList. meta o nil 1))

  IEmptyableCollection
  (-empty [this] this)

  ISeqable
  (-seq [coll] nil)

  Object
  (hashCode [_] 1)
  (equals [_ o]
    (and (or (isa? ISequential o)
             (isa? java.util.List o))
         (nil? (seq o))))
  (toString [_]
    "()")

  IPrintable
  (-pr-seq [_ opts]
    '("()"))

  IEquiv
  (-equiv [this o] (.equals this o))

  ICounted
  (-count [coll] 0))
