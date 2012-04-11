(ns brochure.lang.protocols)

(defprotocol IFn
  (-invoke
    [this]
    [this a]
    [this a b]
    [this a b c]
    [this a b c d]
    [this a b c d e]
    [this a b c d e f]
    [this a b c d e f g]
    [this a b c d e f g h]
    [this a b c d e f g h i]
    [this a b c d e f g h i j]
    [this a b c d e f g h i j k]
    [this a b c d e f g h i j k l]
    [this a b c d e f g h i j k l m]
    [this a b c d e f g h i j k l m n]
    [this a b c d e f g h i j k l m n o]
    [this a b c d e f g h i j k l m n o p]
    [this a b c d e f g h i j k l m n o p q]
    [this a b c d e f g h i j k l m n o p q s]
    [this a b c d e f g h i j k l m n o p q s rest]))

(defprotocol ICounted
  (-count [coll] "constant time count"))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]))

(defprotocol ISeq
  (-first [coll])
  (-rest [coll]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found]))

(defprotocol IAssociative
  (-contains-key? [coll k])
  (-assoc [coll k v]))

(defprotocol IMap
  (-dissoc [coll k]))

(defprotocol ISet
  (-disjoin [coll v]))

(defprotocol IVector)

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o]))

(defprotocol ISeqable
  (-seq [o]))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))

(defprotocol IReversible
  (-rseq [coll]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol IDeref
  (-deref [o]))

(defprotocol IDerefWithTimeout
  (-deref-with-timeout [o msec timeout-val]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o meta]))

(defprotocol INamed
  (-name [o])
  (-namespace [o]))

(defprotocol IReduce
  (-reduce [coll f] [coll f start]))

(defprotocol IEditableCollection
  (-as-transient [coll]))

(defprotocol ISequential)

;;not sure if.
(defprotocol IList)

(defprotocol IPersistent)

(defprotocol IRecord)

(defprotocol IPrintable
  (-pr-seq [o opts]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key]))

(defprotocol IResettable
  (-reset! [this new-value]))

(defprotocol IAtomicallyMutable
  (-compare-and-set! [this old-value new-value]))

(defprotocol IValidable
  (-set-validator! [this validator])
  (-get-validator [this])
  (-validate [this args]))
