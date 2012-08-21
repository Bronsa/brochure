(set! *warn-on-reflection* true)

(ns clojure.lang.kw
  (:refer-clojure :exclude [deftype symbol intern find keyword keyword?])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.sym :refer [symbol]]
            [clojure.lang.utils :refer [clear-cache]]
            [clojure.lang.traits :refer [AFn gen-invoke]]
            [brochure.def :refer [deftype]])
  (:import java.util.concurrent.ConcurrentHashMap
           (java.lang.ref ReferenceQueue SoftReference
                          Reference WeakReference)
           clojure.lang.sym.Symbol))

(defn throw-arity [this & _]
  (throw (IllegalStateException. (str "Wrong number of args passed to keyword: " this))))

(def KWFn
  (cons []
    (list* '(run [this] (UnsupportedOperationException.))
     '(-invoke [this obj]
               (if (instance? ILookup)
                     (-lookup obj this)
                     (get obj this)))
     '(-invoke [this obj not-found]
               (if (instance? ILookup)
                 (-lookup obj this not-found)
                 (get obj this not-found)))
     (take 17 (gen-invoke `throw-arity 'this)))))


(deftype Keyword [^Symbol sym hash ^:unsynchronized-mutable _str]

  :defaults [AFn KWFn]

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :sym sym
      not-found))
  
  Comparable
  (compareTo [this o]
    (.compareTo sym (:sym o)))
  
  INamed
  (-name [this] (-name sym))
  (-namespace [this] (-namespace sym ))
  
  Object
  (toString [this] ;;toString caching
    (if _str
      _str
      (set! _str (.intern (str ":" sym)))))
  (hashCode [this] hash))

(defn make-keyword [^Symbol sym]
  (Keyword. sym (+ (.hashCode sym) 0x9e3779b9) nil))

(def ^ConcurrentHashMap ^:private kw-table (ConcurrentHashMap.))
(def ^:private ref-queue (ReferenceQueue.))

(defn intern
  ([sym]
     (if (string? sym)
       (recur (symbol sym))
       (let [sym (if (-meta sym) (-with-meta sym nil) sym)]
         (clear-cache ref-queue kw-table)
         (let [k (make-keyword sym)]
           (if-let [^Reference existing-ref (.putIfAbsent kw-table sym (WeakReference. k ref-queue))]
             (if-let [existing-k (.get existing-ref)]
               existing-k
               (do (.remove kw-table sym existing-ref) ;;entry died in the interim, do over
                   (intern sym)))
             k)))))
  ([ns name] (intern (symbol ns name))))

(defn keyword? [o] (instance? Keyword o))

(defn keyword
  ([sym] (if (keyword? sym) sym (intern sym)))
  ([ns name] (intern ns name)))

(defn find
  ([sym]
     (if (string? sym)
       (recur (symbol sym))
       (when-let [^Reference ref (.get kw-table sym)]
         (.get ref))))
  ([ns name] (find (symbol ns name))))
