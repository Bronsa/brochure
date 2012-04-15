(ns brochure.lang
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang.utils :refer :all]
            [brochure.lang.PersistentList :refer :all])
  (:import java.util.concurrent.atomic.AtomicReference
           brochure.lang.PersistentList.PersistentList))

(deftype Atom [^AtomicReference state
               ^:unsynchronized-mutable meta
               ^:volatile-mutable validator
               ^:volatile-mutable watches]

  I-IMeta-mutable
  I-IResetMeta
  I-IWatchable
  I-IEquiv
  I-IHash
  
  IDeref
  (-deref [_] (.get state))

  IPrintable
  (-pr-seq [this opts]
    (concat ["#<Atom: "] (-pr-seq state opts) [">"]))

  IAtomicallyMutable
  (-reset! [this new-value]
    (-validate this new-value)
    (let [old-value (-deref this)]
      (.set state new-value)
      (-notify-watches this old-value new-value)
      new-value))
  
  (-compare-and-set! [this old-value new-value]
    (-validate this new-value)
    (when (.compareAndSet state old-value new-value)
      (-notify-watches this old-value new-value)
      true)))

(deftype Delay [^:unsynchronized-mutable val ^:unsynchronized-mutable f]

  IDeref
  (^:synchronized -deref [_]
    (when f
      (set! val (f))
      (set! f nil))
    val)

  IPending
  (^:synchronized -realized? [_]
    (nil? f)))

(deftype Box [val])

(deftype StringSeq [s i meta]
  ASeq

  IWithMeta
  (-with-meta [this new-meta]
    (if (= new-meta meta)
      this
      (StringSeq. s i new-meta)))

  ISeq
  (-first [_]
    (Character/valueOf (.charAt s i)))
  (-next [_]
    (when (< (inc i) (.length s))
      (StringSeq. s (inc i) meta)))

  IIndexedSeq
  (-index [_] i)
  
  ICounted
  (-count [_]
    (- (.length s) i)))

(extend-protocol ISeqable
  String
  (-seq [string] (StringSeq. ^CharSequence string 0 {})))

;;;;;;;
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
;;;;;;;

(declare ->EnumerationSeq)
(defn enum-create [^java.util.Enumeration iter]
  (if (.hasMoreElements iter)
    (let [state (State. nil nil)
          _ (-val! state state)
          _ (-rest! state state)]
      (->EnumerationSeq iter state {}))))

(deftype EnumerationSeq [^java.util.Enumeration iter state meta]
  
  ASeq

  ISequential
   
  ISeq
  (-first [_]
    (when (= (-val state) state)
      (locking state
        (if (= (-val state) state)
          (-val! state (.nextElement iter)))))
    (-val state))
  (-next [this]
    (when (= (-rest state) state)
      (locking state
        (when (= (-rest state) state)
          (-rest! state (enum-create iter)))))
    (-rest state))

  IWithMeta
  (-with-meta [_ new-meta]
    (EnumerationSeq. iter state new-meta)))

(deftype SeqEnumeration [^:unsynchronized-mutable seq]
  java.util.Enumeration
  (hasMoreElements [this]
    (not (nil? seq)))
  (nextElement [this]
    (let [ret (-first seq)]
      (set! seq (-next seq))
      ret)))
