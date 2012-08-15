(ns clojure.lang.ns
  (:refer-clojure :exclude [*ns* intern find-ns ns-aliases ns-unalias the-ns ns-map ns-resolve deftype])
  (:require [clojure.lang.commons :refer [*ns* warning default-aliases]]
            [clojure.lang.traits :refer [AReference]]
            [clojure.lang.protocols :refer :all]
            [clojure.lang.var :refer [create-var]]
            [brochure.def :refer [deftype]])
  (:import (clojure.lang RT ILookup
                         var.Var)))

(declare ns-map warn-or-fail-on-replace)

(deftype Namespace [name mappings aliases ^:unsynchronized-mutable meta]

  :defaults [AReference]
  
  Object
  (toString [this] (str name))

  ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :name name
      :mappings mappings
      :aliases aliases
      :meta (-meta this)
      not-found))

  INamespace
  (-intern-sym [this sym]
    (when (namespace sym)
      (throw (IllegalArgumentException. "Can't intern namespace-qualified symbol")))
    (if-let [o ((ns-map this) sym)]
      (if (= (:ns o) this)
        o
        (let [v (create-var this sym)]
          (warn-or-fail-on-replace this sym o v)
          (swap! (ns-map this) assoc sym v)))
      (let [v (create-var this sym)]
        (swap! (ns-map this) assoc sym v)))))

(defn make-ns [name]
  (Namespace. name (atom {}) (atom default-aliases) nil))

;; ConcurrentHashMap
(defonce namespaces (atom {'clojure.core (make-ns 'clojure.core)
                           'user         (make-ns 'user)}))

(defn find-ns [ns]
  (@namespaces ns))

(defn the-ns [ns]
  (if (instance? Namespace ns)
    ns
    (or (find-ns ns)
        (throw (Exception. (str "Namespace " ns " not found"))))))

(defn- warn-or-fail-on-replace [this sym o v]
  (if (instance? Var o)
    (let [ns (:ns o)]
      (if-not (and (= ns this)
                   (= ns (the-ns 'clojure.core)))
        (throw (IllegalStateException.
                (str sym " already refers to:" o " in namespace: " (:name this))))))
    (warning {} sym " already refers to:" o " in namespace: " (:name this)
             ", being replaced by: " v)))

(defn set-namespace [name ns]
  (swap! namespaces assoc name ns))

(defn ns-aliases [ns]
  @(:aliases (the-ns ns)))

(defn ns-map [ns]
  @(:mappings (the-ns ns)))

(defn ns-unalias [ns sym]
  (swap! (the-ns ns) update-in [:aliases] dissoc sym))

(defn ns-alias
  ([sym] (ns-alias *ns* sym))
  ([ns sym] ((ns-aliases ns) sym)))

(defn resolve-ns
  ([sym] (resolve-ns *ns* sym))
  ([in-ns sym]
     (or (ns-alias in-ns sym)
         (:name (find-ns sym)))))

(defn find-interned-var
  ([sym] (find-interned-var *ns* sym))
  ([ns sym]
     (when-let [o ((ns-map ns) sym)]
       (when (and (instance? Var o)
                  (= ns (.name (.ns ^Var o))))
         o))))

(defn maybe-resolve [ns sym]
  (if-not (nil? (namespace sym))
    (when-let [ns (resolve-ns ns sym)]
      (find-interned-var ns (symbol (name sym))))
    (if (or (and (pos? (-> sym name (.indexOf ".")))
                 (not  (-> sym name (.endsWith "."))))
            (= \[ (-> sym name (.charAt 0))))
      (RT/classForName (name sym))
      (if (= sym 'ns)
        #'ns
        (if (= sym 'in-ns)
          #'in-ns
          ((ns-map ns) sym))))))

(defn ns-resolve
  ([ns sym] (ns-resolve ns nil sym))
  ([ns env sym]
     (when-not (contains? env sym)
       (maybe-resolve (the-ns ns) sym))))
