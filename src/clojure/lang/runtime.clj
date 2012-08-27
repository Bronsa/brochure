(set! *warn-on-reflection* true)

(ns clojure.lang.runtime
  (:refer-clojure :exclude [deftype *ns* symbol hash-combine thread-bound? bound?
                            get-thread-bindings push-thread-bindings pop-thread-bindings])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable AFn AVMutable gen-invoke]]
            [clojure.lang.utils :refer [hash-code hash-combine]]
            [brochure.def :refer [deftype]])
  (:import java.util.concurrent.atomic.AtomicInteger))


;;;;;;;; SYMBOL ;;;;;;;;


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
    (-lookup o this)) ;; instead of RT/getFrom, we extend to ILookup
  (-invoke [this o not-found]
    (-lookup o this not-found))

  ;; ILookup
  ;; (-lookup [this k]
  ;;   (-lookup this k nil)) 
  ;; (-lookup [this k not-found]
  ;;   (case k
  ;;     :name name
  ;;     :namespace ns
  ;;     not-found))
  
  ;; clojure.lang.ILookup
  ;; (valAt [this k]
  ;;   (.valAt this k nil))
  ;; (valAt [this k not-found]
  ;;   (-lookup this k not-found))
  
  Object
  (toString [this] ;;toString caching
    (if _str
      _str
      (set! _str (if ns (.intern (str ns "/" name)) (.intern name)))))
  (equals [this o]
    (or (identical? this o)
        (and (instance? Symbol o)
             (identical? (.name ^Symbol o) name)
             (identical? (.ns ^Symbol o) ns))))
  (hashCode [this] hash)

  Comparable
  (compareTo [this o]
    (cond
      (.equals ^Symbol this o) 0
      
      (and (nil? ns)
           (.ns ^Symbol o))
      -1
      
      :else
      (or (if ns
            (if (nil? (.ns ^Symbol o))
              1
              (let [nsc (.compareTo ns (.ns ^Symbol o))]
                (if (not (zero? nsc))
                  nsc))))
          (.compareTo name (.name ^Symbol o))))))

(defn make-sym [ns ^String name meta]
  (let [hash (hash-combine (.hashCode name)
                           (hash-code ns))]
    (Symbol. ns name hash meta nil)))

(defn symbol
  ([name] (symbol nil name))
  ([^String ns ^String name]
     (make-sym (if ns (.intern ns))
               (.intern name)
               nil)))


;;;;;;;; VAR ;;;;;;;;


(deftype ThreadBox [^:volatile-mutable value thread]
  :defaults [AVMutable])

(defn unbound-throw-arity [this & _]
  (throw (IllegalStateException. (str "Attempting to call unbound fn:" this))))

(def UnboundFn
  (cons []
    (gen-invoke `unbound-throw-arity 'this)))

(defn var-str [#_Namespace ns sym]
  (if ns
      (str "#'" (:name ns) "/" sym)
      (str "#<Var: " (if sym sym "--unnamed--") ">")))

(deftype UnboundVar [#_Namespace ns ^Symbol sym]

  :defaults [AFn UnboundFn]

  Object
  (toString [this] (str "Unbound: " (var-str ns sym))))

(deftype Frame [bindings prev])

(defn make-frame
  ([] (Frame. {} nil))
  ([bindings] (Frame. bindings nil))
  ([bindings prev] (Frame. bindings prev)))

(defn clone-frame [^Frame frame]
  (make-frame (.bindings frame)))

(defn unbound? [v]
  (instance? UnboundVar v))

(declare thread-bound?)

(def ^Frame dynamic-vals (make-frame))

(defn ^ThreadBox get-thread-binding [var]
  (when (thread-bound? var)
    ((.bindings dynamic-vals) var)))

(def VarFn
  (cons []
    (gen-invoke `-invoke '(-deref this))))

;; should this be public?
(defprotocol IVar
  (-bind-root [var root])
  (-alter-root [var fn args])
  (-get-raw-root [var]))

(deftype Var [^Symbol sym #_Namespace ns
              ^AtomicInteger thread-bound-depth
              ^:volatile-mutable root
              ^:unsynchronized-mutable meta
              ^:volatile-mutable validator
              ^:volatile-mutable watches]

  :defaults [AReference AWatchable AValidable AFn VarFn]

  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (case k
      :name sym
      :ns ns
      :root root
      :meta (-meta this)
      not-found))
  
  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (-lookup this k not-found))
  
  INamed
  (-name [this]
    (.name sym))
  (-namespace [this]
    (.name ^Symbol (:name ns)))
  
  IVar
  (-bind-root [this new-root]
    (locking this
      (-validate this new-root)
      (let [old-root root]
        (set! root new-root)
        (-reset-meta! this (dissoc (-meta this) :macro))
        (-notify-watches this old-root new-root))))
  (-alter-root [this fn args]
    (locking this
      (let [new-root (apply fn root args)
            old-root root]
        (-validate this new-root)
        (set! root new-root)
        (-notify-watches this old-root new-root))))
  (-get-raw-root [var] root)
  
  Object
  (toString [_]
    (var-str ns sym))

  IDeref
  (-deref [this]
    (if (thread-bound? this)
      (get-field (get-thread-binding this))
      root))

  IValidable
  (-set-validator! [this new-validator]
    (when-not (unbound? root)
      (-validate new-validator root))
    (set! validator new-validator))

  ISettable
  (-set! [this new-val]
    (-validate this new-val)
    (if-let [box (get-thread-binding this)]
      (if (= (Thread/currentThread)
             (.thread box))
        (set-field! box new-val)
        (throw (IllegalStateException. (str "Can't set!: " sym " from non-binding thread"))))
      (throw (IllegalStateException. (str "Can't change/estabilish root bindings of: " sym " with set!")))))

  IResetMeta
  (-reset-meta! [this m]
    (locking this
      (set! meta (assoc m :name sym :ns ns)))))

(defn thread-bound? [^Var var]
  (pos? (.get ^AtomicInteger (.thread-bound-depth var))))

(defn get-thread-binding-frame []
  (if dynamic-vals
    dynamic-vals
    (make-frame)))

(defn clone-thread-binding-frame []
  (if dynamic-vals
    (clone-frame dynamic-vals)
    (make-frame)))

(defn reset-thread-binding-frame [frame]
  (set! dynamic-vals frame))

(defn create-var
  ([] (create-var (UnboundVar. nil nil)))
  ([root] (create-var nil nil root))
  ([ns sym] (create-var ns sym nil))
  ([ns sym root] (Var. sym ns (AtomicInteger. 0) root nil nil nil)))

(defn bound? [var]
  (or (not (unbound? var))
      (and (thread-bound? var)
           (-> dynamic-vals .bindings (contains? var)))))

(defn push-thread-bindings [bindings]
  (loop [[^Var var val] (first bindings) rest (next bindings) bindings (.bindings dynamic-vals)]
    (if rest
      (do (when-not (:dynamic (-meta var))
            (throw (IllegalStateException. (str "Can't dynamically bind non-dynamic var: "
                                                (:ns var) "/" (:name var)))))
          (-validate var val)
          (.incrementAndGet ^AtomicInteger (.thread-bound-depth var))
          (recur (first rest) (next rest) (assoc bindings var (ThreadBox. (Thread/currentThread) val))))
      (set! dynamic-vals (make-frame bindings dynamic-vals)))))

(defn pop-thread-bindings []
  (when-not (.prev dynamic-vals)
    (throw (IllegalStateException. "Pop without matching push")))
  (doseq [[^Var var _] (.bindings dynamic-vals)]
    (.decrementAndGet ^AtomicInteger (.thread-bound-depth var)))
  (set! dynamic-vals (.prev dynamic-vals)))

(defn get-thread-bindings []
  (let [bindings (.bindings dynamic-vals)]
    (loop [[var box] (first bindings) rest (next bindings) ret {}]
      (if rest
        (recur (first rest) (next rest) (assoc ret var (get-field box)))
        ret))))

(defn intern-symbol
  ([ns sym] (-intern-sym ns sym))
  ([ns sym root] (intern-symbol ns sym root true))
  ([ns sym root replace-root?]
     (let [dvout (intern-symbol ns sym)]
       (when (or (unbound? dvout) replace-root?)
         (-bind-root dvout root))
       dvout)))
