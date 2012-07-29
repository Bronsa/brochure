(ns clojure.lang.ns
  (:refer-clojure :exclude [*ns* find-ns ns-aliases ns-unalias the-ns])
  (:require [clojure.lang.runtime :refer [*ns*]]))

(defrecord Namespace [name mappings aliases])

(defonce namespaces (atom '{clojure.core #clojure.lang.ns.Namespace{:name clojure.core}
                            user         #clojure.lang.ns.Namespace{:name user}}))


(defn find-ns [ns]
  (@namespaces ns))

(defn the-ns [ns]
  (if (instance? Namespace ns)
    ns
    (or (find-ns ns)
        (throw (Exception. (str "Namespace " ns " not found"))))))

(defn set-namespace [name ns]
  (swap! namespaces assoc name ns))

(defn ns-aliases [ns]
  (:aliases (the-ns ns)))

(defn ns-unalias [ns sym]
  (let [ns (the-ns ns)
        unaliased (assoc ns :aliases
                         (dissoc (ns-aliases ns) sym))]
    (reset! namespace unaliased)))

(defn ns-alias
  ([sym] (ns-alias *ns* sym))
  ([ns sym] ((ns-aliases ns) sym)))

(defn resolve-ns
  ([sym] (resolve-ns *ns* sym))
  ([in-ns sym]
     (or (ns-alias in-ns sym)
         (:name (find-ns ns)))))
