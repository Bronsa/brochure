(ns brochure.lang.agent
  (:refer-clojure :exclude [release-pending-sends deftype])
  (:require [clojure.lang.protocols :refer :all]
            [clojure.lang.traits :refer [AReference AWatchable AValidable]]
            [clojure.lang.atom :refer [->Atom]]
            [brochure.def :refer [deftype]])
  (:import (java.util.concurrent atomic.AtomicLong Executors ExecutorService  ThreadFactory
                                 atomic.AtomicReference)))

(defprotocol IError
  (-error [this])
  (-error-mode [thi])
  (-error-mode! [this k])
  (-error-handler [this])
  (-error-handler! [this f]))

(defprotocol IRestartable
  (-restart [this new-state clear?]))

(defprotocol IEnqueueable
  (-enqueue [this action]))

(defprotocol Executable
  (-execute [this]))

(deftype ActionQueue [queue error]
  IEmptyableCollection
  (-empty [_] (ActionQueue. clojure.lang.PersistentQueue/EMPTY nil)))

;; (set! ActionQueue/empty ..)
(def empty-action-queue
  (ActionQueue. clojure.lang.PersistentQueue/EMPTY nil))

(def ^AtomicLong send-thread-pool-counter (AtomicLong. 0))
(def ^AtomicLong send-off-thread-pool-counter (AtomicLong. 0))
(def ^ThreadLocal nested (ThreadLocal.))

(defn ^:private thread-factory [format-string ^AtomicLong counter]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable)
        (.setName (format format-string (.getAndIncrement counter)))))))

(def ^ExecutorService pooled-executor
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))
   (thread-factory "clojure-agent-send-pool-%d" send-thread-pool-counter)))

(def ^ExecutorService solo-executor
  (Executors/newCachedThreadPool
   (thread-factory "clojure-agent-send-off-pool-%d" send-off-thread-pool-counter)))

(defn shutdown []
  (.shutdown solo-executor)
  (.shutdown pooled-executor))

(declare release-pending-sends-)

(deftype Agent [^:volatile-mutable state
                ^AtomicReference action-queue ;;AtomicRefernce<ActionQueue> EMPTY in constructor
                ^:volatile-mutable error-mode ;; :continue
                ^:volatile-mutable error-handler ;; nil
                ^:unsynchronized-mutable meta
                ^:volatile-mutable validator
                ^:volatile-mutable watches]

  :defaults [AReference AWatchable AValidable]
  
  ISettable
  (-set! [this new-state]
    (-validate this new-state)
    (when (not= state new-state)
      (set! state new-state)
      true))

  IDeref
  (-deref [_] state)

  IError
  (-error [_] (.error ^ActionQueue (.get action-queue)))
  (-error-mode [_] error-mode)
  (-error-mode! [_ k] (set! error-mode k))
  (-error-handler [_] error-handler)
  (-error-handler! [_ f] (set! error-handler f))

  IRestartable
  (^:synchronized -restart [this new-state clear?]
    (when-not (-error this)
      (throw (RuntimeException. "Agent does not need a restart")))
    (-validate this new-state)
    (if clear?
      (.set action-queue empty-action-queue)
      (loop [restarted false ^ActionQueue prior nil]
        (if-not restarted
          (let [^ActionQueue prior (.get action-queue)]
            (recur (.compareAndSet action-queue prior (ActionQueue. (.queue prior) nil)) prior))
          (if (pos? (-> prior .queue count))
            (-execute ^Action (-> prior .queue peek))))))
    new-state)

  IEnqueueable
  (-enqueue [this action]
    (loop [queued false ^ActionQueue prior nil]
      (if-not queued
        (let [^ActionQueue prior (.get action-queue)]
          (recur (.compareAndSet action-queue prior (ActionQueue. (-> prior .queue (cons action))
                                                                  (.error prior))) prior))
        (if (and (zero? (-> prior .queue count)) (nil? (.error prior)))
          (-execute action))))))

(deftype Action [^Agent agent fn args solo?]

  Runnable
  (run [this]
    (try
      (.set nested [])
      (let [error (->Atom nil {} nil nil)]
        (try (let [old-value (-deref agent)
                   new-value (apply fn (-deref agent) args)]
               (-set! agent new-value)
               (-notify-watches agent old-value new-value))
         (catch Throwable e
           (-set! error e)))
        (if-not (-deref error)
          (release-pending-sends-)
          (do (.set nested nil)
              (if-let [error-handler (-error-handler agent)]
                (try (error-handler agent error)
                     (catch Throwable _)))
              (if (= :continue (-error-mode agent))
                (-set! error nil))))
        (loop [popped false ^ActionQueue next nil]
          (if-not popped
            (let [^ActionQueue prior (.get ^AtomicReference (.action-queue agent))
                  next (ActionQueue. (pop (.queue prior)) (-deref error))]
              (recur (.compareAndSet ^AtomicReference (.action-queue agent) prior next) next))
            (if (and (nil? (-deref error))
                     (pos? (-> next .queue count)))
              (-execute (-> next .queue peek))))))
        (finally
          (.set nested nil))))

  Executable
  (-execute [this]
    (try (.execute ^ExecutorService (if solo? solo-executor pooled-executor) this)
      (catch Throwable error
        (if-let [error-handler (-error-handler agent)]
          (try (error-handler agent error)
               (catch Throwable _)))))))

(defn dispatch-action [^Action action]
  (let [trans (clojure.lang.LockingTransaction/getRunning)]
    (cond
      trans         (.enqueue trans action)
      (.get nested) (.set nested (-> nested .get (cons action)))
      :else         (-enqueue (.agent action) action))))

(defn dispatch [agent f args solo?]
  (if-let [error (-error agent)]
    (throw (RuntimeException. "Agent is failed, needs restart" error)))
  (dispatch-action (Action. agent f args solo?))
  agent)

(defn release-pending-sends []
  (if-let [sends (.get nested)]
    (do (doseq [^Action a sends]
          (-enqueue (.agent a) a))
        (.set nested [])
        (count sends))
    0))

(defn queue-count [agent]
  (-> agent .action-queue .get .queue count))
