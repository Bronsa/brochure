(ns brochure.lang.Keyword
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang.utils :refer :all])
  (:import (java.io ObjectStreamException Serializable)
           (java.lang.ref Reference WeakReference SoftReference ReferenceQueue)
           java.util.concurrent.ConcurrentHashMap))

(def ^:private table (ConcurrentHashMap.))

(def hash-code
  (memoize
   (fn [sym]
     (+ 0x9e3779b9 (.hashCode sym)))))

(defn intern
  ([name]
     (if-not (symbol? name)
       (intern (symbol name))
       ))
  ([ns name]))

(deftype Keyword [refernce-queue symbol]
  )
