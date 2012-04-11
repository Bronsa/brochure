(ns brochure.lang.PersistentList
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang.utils :refer :all]))

(set! *unchecked-math* true) ;; we need this to overflow on -hash

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


(declare ->EmptyList)

(deftype Cons [first rest meta]

  ASeq
  I-List

  ISeq
  (-first [coll] first)
  (-rest [coll] (if (nil? rest) (->EmptyList {}) rest))

  IEmptyableCollection
  (-empty [_] (with-meta (->EmptyList) meta))

  ICollection
  (-conj [coll o] (Cons. nil o coll))

  ICounted
  (-count [_] (inc (-count rest)))
  
  IWithMeta
  (-with-meta [_ new-meta] (Cons. meta first rest)))

;; (defn identical? [a b]) == java operator
(deftype PersistentList [first rest count meta] ;rest is never nil

  ASeq
  I-List

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
