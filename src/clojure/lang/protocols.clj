(ns clojure.lang.protocols)

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
    [this a b c d e f g h i j k l m n o p q s rest])
  (-apply [this arglist]))

(defprotocol IMutableField
  (set-field! [this val])
  (get-field [this]))

(defprotocol IMeta
  (-meta [o]))

(defprotocol IWithMeta
  (-with-meta [o new-meta]))

(defprotocol IResetMeta
  (-reset-meta! [o new-meta]))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol IValidable
  (-set-validator! [this validator])
  (-get-validator [this])
  (-validate [this args]))

(defprotocol ISettable
  (-set! [this new-value]))

(defprotocol IDeref
  (-deref [o]))

(defprotocol IWatchable
  (-notify-watches [this oldval newval])
  (-add-watch [this key f])
  (-remove-watch [this key])
  (-watches [this]))

(defprotocol IAtomicallyMutable
  (-compare-and-set! [this old-value new-value])
  (-reset! [this new-value]))

(defprotocol IPending
  (-realized? [d]))

(defprotocol INamespace
  (-intern-sym [ns sym]))

(defprotocol IVar
  (-bind-root [var root])
  (-alter-root [var fn args])
  (-get-raw-root [var]))

(defprotocol INamed
  (-name [o])
  (-namespace [o]))

(defprotocol ILookup
  (-lookup [o k] [o k not-found])) ;;valAt in java

(defprotocol ICounted
  (-count [coll] "constant time count"))

(defprotocol IIndexed
  (-nth [coll n] [coll n not-found]))

(defprotocol ISeq
  (-first [coll])
  (-next [coll]))

(defprotocol IIndexedSeq
  (-index [seq]))

(defprotocol IEquiv
  (-equiv [o other]))

(defprotocol IHash
  (-hash [o])) ;; hasheq in java

(defprotocol ISeqable
  (-seq [o]))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll o]))
