(set! *warn-on-reflection* true)

(ns clojure.lang.var
  (:refer-clojure :exclude [deftype atom swap!])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable AFn AVMutable gen-invoke]]
            [clojure.lang.atom :refer [atom swap!]]
            [brochure.def :refer [deftype]])
  (:import java.util.concurrent.atomic.AtomicInteger))

(deftype ThreadBox [^:volatile-mutable value thread]
  :defaults [AVMutable])

(declare throw-arity)

(def UnboundFn
  (cons []
    (gen-invoke `throw-arity)))

(defn var-str [ns sym]
  (if ns
      (str "#'" (:name ns) "/" sym)
      (str "#<Var: " (if sym sym "--unnamed--") ">")))

(deftype UnboundVar [ns sym]

  :defaults [AFn UnboundFn]

  Object
  (toString [this] (str "Unbound: " (var-str ns sym))))

(deftype Frame [bindings prev])

;; (def rev (atom 0)) what is this used for?

(defn make-frame
  ([] (Frame. {} nil))
  ([bindings] (Frame. bindings nil))
  ([bindings prev] (Frame. bindings prev)))

(defn clone-frame [^Frame frame]
  (make-frame (.bindings frame)))

(defn throw-arity [^UnboundVar this n]
  (throw (IllegalStateException. (str "Attempting to call unbound fn:" (.var this)))))

(defn unbound? [v]
  (instance? UnboundVar v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol INamespace
  (-intern-sym [ns sym]))

(defprotocol IVar
  (-bind-root [var root])
  (-alter-root [var fn args])
  (-get-raw-root [var]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare thread-bound? get-thread-binding)

(deftype Var [sym ns
              ^AtomicInteger thread-bound-depth
              ^:volatile-mutable root
              ^:unsynchronized-mutable meta
              ^:volatile-mutable validator
              ^:volatile-mutable watches]

  :defaults [AReference AWatchable AValidable]

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

(def dynamic-vals (make-frame))

(defn thread-bound? [var]
  (pos? (.get (.thread-bound-depth var))))

(defn get-thread-binding [var]
  (when (thread-bound? var)
    ((.bindings dynamic-vals) var)))

(defn get-thread-binding-frame []
  (if dynamic-vals
    dynamic-vals
    (make-frame)))

(defn clone-thread-binding-frame []
  (if dynamic-vals
    (clone-frame dynamic-vals)
    (make-frame)))

(defn reset-thread-binding-frame [frame]
  (set! dynamic-vals ^Frame frame))

(defn create-var
  ([] (create-var (UnboundVar. nil nil)))
  ([root] (Var. nil nil (AtomicInteger. 0) root {} nil nil)))

(defn bound? [var]
  (or (not (unbound? var))
      (and (thread-bound? var)
           (-> dynamic-vals .bindings (contains? var)))))

;; (defn push-thread-bindings [bindings]
;;   (let [bindings (.bindings dynamic-vals)]
    
;;     (set! dynamic-vals (make-frame bindings dynamic-vals))))

;; (defn pop-thread-bindings [])

(defn intern
  ([ns sym] (-intern-sym ns sym))
  ([ns sym root] (intern ns sym root true))
  ([ns sym root replace-root?]
     (let [dvout (intern ns sym)]
       (when (or (unbound? dvout) replace-root?)
         (-bind-root dvout root))
       dvout)))
