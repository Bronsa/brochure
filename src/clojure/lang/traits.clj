(set! *warn-on-reflection* true)

(ns clojure.lang.traits
  (:require [brochure.def :refer [deftrait]]
            [clojure.lang.protocols :refer :all]
            [clojure.lang.utils])) ;; ensure it's loaded

(deftrait AReference [^:unsynchronized-mutable meta]
  IMeta
  (-meta [this]
    (locking this meta))
  
  IResetMeta
  (-reset-meta! [this m]
    (locking this (set! meta m))))

;; not going to work.
;; we need to implement our own doseq, assoc, dissoc
(deftrait AWatchable [^:volatile-mutable watches]
  IWatchable
  (-notify-watches [this old-value new-value]
    (doseq [[key f] watches]
      (f key this old-value new-value)))
  (-add-watch [this key f]
    (locking this
      (set! watches (assoc watches key f))))
  (-remove-watch [this key]
    (locking this
      (set! watches (dissoc watches key))))
  (-watches [this] watches))

(deftrait AValidable [^:volatile-mutable validator]
  IValidable
  (-set-validator! [this new-validator]
    (when (new-validator (-deref this))
      (set! validator new-validator)))
  (-get-validator [_] validator)
  (-validate [_ new-value]
    (when validator
      (assert (validator new-value) "Validator rejected reference state"))))
;; (IllegalStateException. "Invalid reference state")

(defn throw-arity [^Object this & args]
  (let [n (count args)
        name (-> this .getClass .getSimpleName)
        suffix (.lastIndexOf name "__")
        elided-name (if (= suffix -1) name (.substring name 0 suffix))]
    (throw (clojure.lang.ArityException. n (.replace elided-name \_ \-)))))

(defn gen-invoke [f this]
  (map (fn [args] (list '-invoke (vec (cons 'this args))
                        (list* f this args)))
          (cons [] (take-while seq (iterate rest (repeat 18 '_))))))

(def AFn
  (cons []
        (into 
         (list
          'java.util.concurrent.Callable
          '(call [this] (-invoke this))
          'java.lang.Runnable
          '(run [this]
                (try (-invoke this)
                     (catch Exception e
                       (throw e))))
          'IFn
          '(-apply [this arglist]
                   (let [arglist-len (count arglist)] 
                     (if (< arglist-len 19)
                       (eval `(-invoke ~this ~@arglist))
                       (eval `(-invoke ~this ~@(take 19 arglist) '~(drop 19 arglist)))))))
         (gen-invoke `throw-arity 'this))))

(deftrait AVMutable [^:volatile-mutable value]
  IMutableField
  (set-field! [this new-val] (set! value new-val))
  (get-field [this] value))

(defmacro eq? [fn]
  `(and (or (satisfies? ISequential obj)
            (instance? java.util.List obj))
         (loop [ms (-seq obj) s (-seq this)]
           (if-not (nil? ms)
             (if (not (and (nil? ms)
                           (~fn (-first this) (-first ms))))
               (recur (-next ms) (-next s)))
             (nil? ms)))))

(deftrait ASeq [hash meta]
  
  ISeq
  (-first [_] first)
  (-next [_] next)

  IHash
  (-hash [this]
    (clojure.lang.utils/seqable-hash-code this))

  IEmptyableCollection
  (-empty [_] clojure.lang.persistent-list/empty)

  ISeqable
  (-seq [this] this)
  
  IPersistentCollection
  (-conj [this o]
    (clojure.lang.persistent-list.Cons. o this))

  Object
  (hashCode [this]
    (clojure.lang.utils/seqable-hash-code this)) ;; s/hash/hash-code
  
  (equals [this obj]
    (eq? clojure.lang.utils/equals?))

  IEquiv
  (-equiv [this obj]
    (eq? clojure.lang.utils/equiv?))

  IMeta
  (-meta [_] meta)
  
  java.util.List
  (contains [this o]
            (boolean (loop [s (-seq this)]
                       (when s
                         (if (clojure.lang.utils/equiv? (-first s) o)
                           true
                           (recur (-next s)))))))
  ;; (containsAll [this c])
  ;; (get [this index] (nth this index))
  (indexOf [this o]
           (loop [coll this i 0]
             (if (-seq coll)
               (or (when (clojure.lang.utils/equiv? o (-first coll)) i)
                   (recur (-next coll) (inc i)))
               -1)))
  (isEmpty [this]
           (nil? (-seq this)))
  (iterator [this]
            (SeqIterator. this))
  (lastIndexOf [this o]
               (.lastIndexOf (clojure.lang.utils/reify-seq this) o))
  (listIterator [this]
                (.listIterator (clojure.lang.utils/reify-seq this)))
  (listIterator [this index]
                (.listIterator (clojure.lang.utils/reify-seq this) index))
  (size [this] (clojure.lang.utils/count this))
  (subList [this fromIndex toIndex]
           (.subList (clojure.lang.utils/reify-seq this) fromIndex toIndex))
  (toArray [this]
           (clojure.lang.utils/seq-to-array this))
  (toArray [this a]
           (clojure.lang.utils/seq-to-array this a))
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
       (throw (UnsupportedOperationException.))))
