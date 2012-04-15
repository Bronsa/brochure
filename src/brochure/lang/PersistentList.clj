(ns brochure.lang.PersistentList
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang.utils :refer :all]))

(deftype SeqIterator [^:unsynchronized-mutable seq]
  java.util.Iterator
  (hasNext [_] (boolean seq))
  (next [_]
    (if seq
      (let [ret (-first seq)]
        (set! seq (-next seq))
        ret)
      (throw (java.util.NoSuchElementException.))))
  (remove [_] (throw (java.util.NoSuchElementException.))))

(declare ->PersistentList)
(deftype EmptyList [meta]

  I-List
  
  IPersistent
  IList
  ISequential

  java.util.List
  (size [_] 0)
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
    o)
  
  IWithMeta
  (-with-meta [this new-meta]
    (if (not= new-meta meta)
      (EmptyList. new-meta)
      this))

  IMeta
  (-meta [coll] meta)

  ISeq
  (-first [coll] nil)
  (-next [coll] nil)

  IStack
  (-peek [coll] nil)
  (-pop [coll] (throw (Exception. "Can't pop empty list")))

  ICollection
  (-conj [coll o]
    (->PersistentList o nil 1 meta))

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

(def empty-list (EmptyList. {}))

(deftype Cons [first next meta]

  ASeq
  I-List

  ISequential 
  java.io.Serializable
  
  IEmptyableCollection
  (-empty [_] (with-meta empty-list meta))

  ICollection
  (-conj [coll o] (Cons. nil o coll))
  
  ICollection
  (-conj [this o]
    (->PersistentList o this (inc (count- this)) meta))
  
  ICounted
  (-count [_] (inc (count- next)))
  
  IWithMeta
  (-with-meta [_ new-meta] (Cons. meta first next)))

(deftype PersistentList [first next count meta]

  IReduce
  (-reduce [_ f]
    (loop [ret first s next]
      (if s
        (recur (f ret (-first s)) (-next s))
        ret)))

  (-reduce [_ f start]
    (loop [ret (f start first) s next]
      (if s
        (recur (f ret (-first s)) (-next s))
        ret)))
    
  ASeq
  I-List

  IPersistent
  IList
  ISequential 
  java.io.Serializable

  IWithMeta
  (-with-meta [_ new-meta]
    (PersistentList. first next new-meta count))

  IStack
  (-peek [_] first)
  (-pop [_] next)

  ICounted
  (-count [_] count)

  Object
  (equals [this o]
    (or (identical? this o)
        (-equiv this o)))
  
  ICollection
  (-conj [this o]
    (PersistentList. o this (inc count) meta))
  
  IEquiv
  (-equiv [this o]
    (and (or (isa? ISequential o) (isa? java.util.List o))
         (every? true? (map equals? this o)))))

(defn create [^java.util.List init]
  (loop [ret empty-list ^java.util.Iterator i (.listIterator init (.size init))]
    (if (.hasPrevious ^java.util.Iterator i)
      (recur (-conj ret (.previous ^java.util.Iterator i)) i)
      ret)))

(def list- (proxy [clojure.lang.RestFn] []
             (getRequiredArity [] 0)
             (doInvoke [args]
               (if (instance? clojure.lang.ArraySeq args)
                 (let [^objects argsarray (.array ^clojure.lang.ArraySeq args)]
                   (loop [i (dec (clojure.lang.RT/alength argsarray)) ret empty-list]
                     (if (pos? i)
                       (recur (dec i) (-conj ret (aget argsarray i)))
                       ret)))
                 (let [^java.util.LinkedList list (java.util.LinkedList.)]
                   (loop [s (seq args)]
                     (when s
                       (.add list (first s))
                       (recur (next s))))
                   (create list))))))
