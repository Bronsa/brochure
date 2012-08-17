(set! *warn-on-reflection* true)

(ns clojure.lang.sym
  (:refer-clojure :exclude [deftype intern hash-combine])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AFn gen-invoke]]
            [clojure.lang.utils :refer [hash-code hash-combine]]
            [brochure.def :refer [deftype]]))

(declare make-sym)

(deftype Symbol [^String ns ^String name hash meta ^:unsynchronized-mutable _str]

  :defaults [AFn]

  IMeta
  (-meta [this] meta)
  IWithMeta
  (-with-meta [this new-meta] (make-sym ns name new-meta))
  
  INamed
  (-name [this] name)
  (-namespace [this] ns)

  IFn
  (-invoke [this o]
    (-lookup o this))
  (-invoke [this o not-found]
    (-lookup o this not-found))
  
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :name name
      #_:ns #_ns ;;returns a string not a ns, wouldn't be coherent
      not-found))
  
  Object
  (toString [this] ;;toString caching
    (if _str
      _str
      (set! _str (if ns (.intern (str ns "/" name)) (.intern name)))))
  (equals [this o]
    (or (= this o)
        (and (instance? Symbol o)
             (= (-name o) name)
             (= (-namespace o) ns))))
  (hashCode [this] hash)

  Comparable
  (compareTo [this o]
    (cond
      (identical? this o) 0

      (and (nil? ns)
           (not (nil? (-namespace o))))
      -1
      
      :else
      (or (if-not (nil? ns)
            (when (nil? (-namespace o))
              1)
            (let [nsc (.compareTo ns (-namespace o))]
              (and (not (zero? nsc)) nsc)))
          (.compareTo name (-name o))))))

(defn make-sym [ns ^String name meta]
  (let [hash (hash-combine (.hashCode name)
                           (hash-code ns))]
    (Symbol. ns name hash meta nil)))

(defn intern [^String ns ^String name]
  (make-sym (and ns (.intern ns))
            (.intern name)
            nil))
