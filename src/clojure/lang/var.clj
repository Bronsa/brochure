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

;; a var is a thread local too
;; (def dynamic-vals
;;   (proxy [ThreadLocal] []
;;     (initalValue [] (make-frame))))

(def dynamic-vals (make-frame))

(defn throw-arity [^UnboundVar this n]
  (throw (IllegalStateException. (str "Attempting to call unbound fn:" (.var this)))))

(defn unbound? [v]
  (instance? UnboundVar v))

(deftype Var [sym ns
              ^AtomicInteger thread-bound-depth
              ^:volatile-mutable root
              ^:unsynchronized-mutable meta
              ^:volatile-mutable validator
              ^:volatile-mutable watches]

  :defaults [AReference AWatchable AValidable]

  Object
  (toString [_]
    (var-str ns sym))

  IGet
  (-get [this]
    (if-not (.get thread-bound?)
      root
      (-deref this)))

  IDeref
  (-deref [this]
    (if (pos? (.get thread-bound-depth))
      (.val (get-thread-binding this))
      root))

  IValidable
  (-set-validator! [this f]
    (when-not (unbound? root)
      (-validate f root))
    (set! validator f)))

(defn get-thread-binding-frame []
  (let [f (.get dvals)]
    (if f
      f
      (make-frame))))

(defn clone-thread-binding-frame []
  (let [f (.get dvals)]
    (if f
      (clone-frame f)
      (make-frame))))

(defn reset-thread-binding-frame [frame]
  (.set dvals ^Frame frame))

(defn create-var
  ([] (create-var (UnboundVar. nil nil)))
  ([root] (Var. nil nil (AtomicBoolean. false) root {} nil nil)))

;; (defn bound? [var]
;;   (or (has-root? var)
;;       (and (.get (.thread-bound? (-> dvals .get .bindings .containsKey this))))))
