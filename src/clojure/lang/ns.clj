(set! *warn-on-reflection* true)

(ns clojure.lang.ns
  (:refer-clojure :exclude [*ns* intern find-ns ns-aliases ns-unalias the-ns ns-map ns-resolve
                            deftype refer intern remove-ns symbol])
  (:require [clojure.lang.commons :refer [warning default-aliases]]
            [clojure.lang.traits :refer [AReference]]
            [clojure.lang.protocols :refer :all]
            [clojure.lang.var :refer [create-var intern]]
            [clojure.lang.sym :refer [symbol]]
            [brochure.def :refer [deftype]])
  (:import java.util.concurrent.ConcurrentHashMap
           (clojure.lang RT
                         var.Var)))

(declare ns-map warn-or-fail-on-replace refer *ns*)

(deftype Namespace [#_Symbol name mappings aliases ^:unsynchronized-mutable meta]

  :defaults [AReference]
  
  Object
  (toString [this] (str name))

  clojure.lang.ILookup
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
    (refer this sym (create-var this sym) true)))

(defn refer
  ([sym v] (refer *ns* sym v))
  ([this sym v & [intern?]]
     (when (-namespace sym)
       (throw (IllegalArgumentException. "Can't intern namespace-qualified symbol")))
     (let [o ((ns-map (:name this)) sym)]
       (if (and o (if intern? (= o v) (= (:ns o) this)))
         o
         (do
           (when o 
             (warn-or-fail-on-replace this sym o v))
           ((swap! (:mappings this) assoc sym v) sym))))))

(defn make-ns [name]
  (if (-namespace name)
    (throw (IllegalArgumentException. "Can't create a namespace out of a namespace-qualified symbol"))) ;; this is legal in clojure-jvm
  (Namespace. name (atom {}) (atom default-aliases) nil))

(defonce ^ConcurrentHashMap ^:private namespaces (ConcurrentHashMap.))

(defn find-ns [ns]
  (.get namespaces ns))

(defn find-or-create [name]
  (if-let [ns (find-ns name)]
    ns
    (let [new-ns (make-ns name)]
      (.putIfAbsent namespaces name new-ns)
      new-ns)))

(defonce clojure-core-sym (symbol "clojure.core"))
(defonce user-sym (symbol "user"))
(defonce *ns*-sym (symbol "*ns*"))
(defonce in-ns-sym (symbol "in-ns"))
(defonce ns-sym (symbol "ns"))

(find-or-create clojure-core-sym)
(find-or-create user-sym)

(defn the-ns [ns]
  (if (instance? Namespace ns)
    ns
    (or (find-ns ns)
        (throw (Exception. (str "Namespace " ns " not found"))))))

(defn- warn-or-fail-on-replace [this sym o v]
  (if (instance? Var o)
    (let [ns (:ns o)]
      (if-not (and (= ns this)
                   (= ns (the-ns clojure-core-sym)))
        (throw (IllegalStateException.
                (str sym " already refers to:" o " in namespace: " (:name this))))))
    (warning {} sym " already refers to:" o " in namespace: " (:name this)
             ", being replaced by: " v)))

(defn set-namespace [name ns]
  (.put namespaces  name ns))

(defn ns-aliases [ns]
  @(:aliases (the-ns ns)))

(defn ns-map [ns]
  @(:mappings (the-ns ns)))

(when-not ((ns-map clojure-core-sym) *ns*-sym)
  (intern (the-ns clojure-core-sym) *ns*-sym (the-ns user-sym))
  (def *ns* ((ns-map clojure-core-sym) *ns*-sym))
  (-reset-meta! *ns* {:dynamic true}))

(defn ns-unalias [ns sym]
  (swap! (the-ns ns) update-in [:aliases] dissoc sym))

;; lookupAlias
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
                  (= ns (:name (:ns o))))
         o))))

(defonce ns-var (intern (the-ns clojure-core-sym) ns-sym false))
(defonce in-ns-var (intern (the-ns clojure-core-sym) in-ns-sym false))

(defn maybe-resolve [ns sym]
  (if-not (nil? (-namespace sym))
    (when-let [ns (resolve-ns ns sym)]
      (find-interned-var ns (symbol (-name sym))))
    (if (or (and (pos? (-> sym -name (.indexOf ".")))
                 (not  (-> sym -name (.endsWith "."))))
            (= \[ (-> sym -name (.charAt 0))))
      (RT/classForName (-name sym))
      (if (= sym ns-sym)
        ns-var
        (if (= sym in-ns-sym)
          in-ns-var
          ((ns-map ns) sym))))))

(defn ns-resolve
  ([ns sym] (ns-resolve ns nil sym))
  ([ns env sym]
     (when-not (contains? env sym)
       (maybe-resolve (the-ns ns) sym))))

(defn different-instances-of-same-class-name? [^Class c1 ^Class c2]
  (and (not= c1 c2)
       (= (.getName c1)
          (.getName c2))))

(defn import-class
  ([c] (import-class *ns* c))
  ([this ^Class c]
     (let [n (.getName c)]
       (import-class (symbol (.substring n (inc (.lastIndexOf n ".")))))))
  ([this sym v]
     (when (-namespace sym)
       (throw (IllegalArgumentException. "Can't intern namespace-qualified symbol")))
     (let [c ((ns-map this) sym)
           c (or (when (or (not c) (different-instances-of-same-class-name? c v))
                   ((swap! (:mappings this) assoc sym v) sym))
                 c)]
       (if (= v c)
         c
         (throw (IllegalStateException.
                 (str sym " already refers to: " c " in namespace " (:name this))))))))

(defn remove-ns [name]
  (if (= name clojure-core-sym)
    (throw (IllegalArgumentException. "Cannot remove clojure.core"))
    (.remove namespaces name)))

(defn find-interned-var
  ([sym] (find-interned-var *ns* sym))
  ([this sym]
     (let [o ((ns-map this) sym)]
       (if (and o (instance? Var o) (= this (:ns o)))
         o))))

(defn add-alias
  ([alias ns] (add-alias *ns* alias ns))
  ([this alias ns]
     (when-not (and alias ns)
       (throw (NullPointerException. "Expecting Symbol + Namespace")))
     (when-not (contains? (ns-aliases this) alias)
       (swap! (:aliases this) assoc alias ns))
     (if-not (= ((ns-aliases this) alias) ns)
       (throw (IllegalStateException.
               (str "Alias: " alias "already exists in namespace "
                    (:name this) ", aliasing" ((ns-aliases this) alias)))))))

(defn remove-alias
  ([alias] (remove-alias *ns* alias))
  ([this alias] (swap! (:aliases this) dissoc alias)))
