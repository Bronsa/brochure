(ns brochure.lang.implementations)

(defimpl I-IMeta-mutable
  IMeta
  (^:synchronized -meta [_] meta))

(defimpl I-IResetMeta
  IWithMeta
  (^:synchronized -reset-meta! [_ new-meta] (set! meta new-meta)))

(defimpl I-IWatchable
  IWatchable
  (-notify-watches [this old-value new-value]
    (doseq [[key f] watches]
      (f key this old-value new-value)))
  (^:synchronized -add-watch [_ key f]
    (set! watches (assoc watches key f)))
  (^:synchronized -remove-watch [_ key]
    (set! watches (dissoc watches key))))

(defimpl I-IValidable
  IValidable
  (-set-validator! [this new-validator]
    (when (new-validator (-deref this))
      (set! validator new-validator)))
  (-get-validator [_] validator)
  (-validate [_ new-value]
    (when validator
      (assert (validator new-value) "Validator rejected reference state"))))

(defimpl I-IEquiv
  IEquiv
  (-equiv [this other] (identical? this other)))

(defimpl I-IHash
  IHash
  (-hash [this] (.hashCode this)))

(defimpl I-List
  java.util.List
  (contains [this o]
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
    (->SeqIterator this))
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
    (throw (UnsupportedOperationException.))))

(defimpl ASeq

  ISeq
  (-first [_] first)
  (-next [_] next)

  IHash
  (-hash [this]
    (int (reduce- #(+ (* 31 %) (hash- %2)) 1 this)))

  IEmptyableCollection
  (-empty [_] (->EmptyList))

  ISeqable
  (-seq [this] this)

  ICollection
  (-conj [this o]
    (->PersistentList o this (inc (count- this)) meta))

  Object
  (hashCode [this]
    (reduce- #(+ (* 31 %) (if %2 (.hashCode %2) 0)) 1 this))

  IMeta
  (-meta [_] meta))


;; (def ARef (merge IMetaImpl IWithMetaImpl IWatchableImpl))

(defn throw-arity [this n]
  (let [name (-> this .getClass .getSmpleName)
        suffix (.lastIndexOf name "__")
        elided-name (if (= suffix -1) name (.substring name 0 suffix))]
    (throw (clojure.lang.ArityException. n (.replace elided-name \_ \-)))))

(def AFn
  ^:impl {'java.lang.Callable
          '[(call [this] (-invoke this))]
          'java.lang.Runnable
          '[(run [this] (-invoke this))]
          'IFn
          (map (fn [args] (list '-invoke (vec (cons 'this args))
                                (list 'throw-arity 'this (count args))))
               (cons [] (take-while seq (iterate rest (repeat 18 '_)))))})

(defmacro apply-to [f arglist]
  (let [arglist-len (count arglist)] ;;inefficent
    (if (< arglist-len 19)
      `(-invoke ~f ~@arglist)
      `(-invoke ~f ~@(take 19 arglist) '~(drop 19 arglist)))))
