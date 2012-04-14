(ns brochure.lang.Agent
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all]
            [brochure.lang :refer :all])
  (:import java.util.concurrent.atomic.AtomicLong
           java.util.concurrent.Executors
           java.util.concurrent.ThreadFactory
           java.util.concurrent.atomic.AtomicReference))

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

(def send-thread-pool-counter (AtomicLong. 0))
(def send-off-thread-pool-counter (AtomicLong. 0))
(def nested (ThreadLocal.))

(defn ^:private thread-factory [format-string counter]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable)
        (.setName (format format-string (.getAndIncrement counter)))))))

(def pooled-executor
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))
   (thread-factory "clojure-agent-send-pool-%d" send-thread-pool-counter)))

(def solo-executor
  (Executors/newCachedThreadPool
   (thread-factory "clojure-agent-send-off-pool-%d" send-off-thread-pool-counter)))

(defn shutdown []
  (.shutdown solo-executor)
  (.shutdown pooled-executor))

(deftype Action [agent fn args solo?]

  Runnable
  (run [this]
    (try
      (.set nested [])
      (let [error (->Atom nil)]
        (try (let [old-value (-deref agent)
                   new-value (apply fn (-deref agent) args)]
               (-reset! agent new-value)
               (-notify-watches agent old-value new-value))
         (catch Throwable e
           (-reset! error e)))
        (if-not (-deref error)
          (release-pending-sends)
          (do (.set nested nil)
              (if-let [error-handler (-error-handler agent)]
                (try (error-handler agent error)
                     (catch Throwable _)))
              (if (= :continue (-error-mode agent))
                (-reset! error nil))))
        (loop [popped false next nil]
          (if-not popped
            (let [prior (-> agent .aq .get)
                  next (ActionQueue. (-> prior .queue .pop) (-deref error))]
              (recur (-> agent .aq (.compareAndSet prior next)) next))
            (if (and (nil? (-deref error))
                     (pos? (-> next .queue .count)))
              (.execute ^Action (-> next .queue .peek))))))
        (finally
          (.set nested nil))))

  Executable
  (-execute [this]
    (try (.execute (if solo? solo-executor pooled-executor) this)
      (catch Throwable error
        (if-let [error-handler (-error-handler agent)]
          (try (error-handler agent error)
            (catch Throwable _)))))))

(defn dispatch-action [action]
  (let [trans (clojure.lang.LockingTransaction/getRunning)]
    (cond
      trans         (.enqueue trans action)
      (.get nested) (.set nested (-> nested .get (.cons action))) ;; -cons once we have our own datastructures
      :else         (-enqueue (.agent action) action))))

(defn dispatch [agent f args solo?]
  (if-let [error (-error agent)]
    (throw (RuntimeException. "Agent is failed, needs restart" error)))
  (dispatch-action (Action. agent f args solo?))
  agent)

(defn release-pending-sends []
  (if-let [sends (.get nested)]
    (do (doseq [a sends]
          (-enqueue (.agent a) a))
        (.set nested [])
        (count sends))
    0))

(deftype Agent [^:volatile-mutable state
                ^AtomicReference action-queue ;;AtomicRefernce<ActionQueue> EMPTY in constructor
                ^:volatile-mutable error-mode ;; :continue
                ^:volatile-mutable error-handler ;; nil
                ^:unsynchronized-mutable meta
                ^:volatile-mutable validator
                ^:volatile-mutable watches]

  I-IMeta-mutable
  I-IResetMeta
  I-IWatchable
  I-IEquiv
  I-IHash
  
  IResettable
  (-reset! [this new-state]
    (-validate this new-state)
    (when (not= state new-state)
      (set! state new-state)
      true))

  IDeref
  (-deref [_] state)

  IError
  (-error [_] (-> action-queue .get .error))
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
      (loop [restarted false prior nil]
        (if-not restarted
          (let [prior (.get action-queue)]
            (recur (.compareAndSet action-queue prior (ActionQueue. (.queue prior) nil)) prior))
          (if (pos? (-> prior .queue .count))
            (.execute ^Action (-> prior .queue .peek))))))
    new-state)

  IEnqueueable
  (-enqueue [this action]
    (loop [queued false prior nil]
      (if-not queued
        (let [prior (.get action-queue)]
          (recur (.compareAndSet action-queue prior (ActionQueue. (-> prior .queue (.cons action)) (.error prior))) prior))
        (if (and (zero? (-> prior .queue .count)) (nil? (.error prior)))
          (.execute action))))))

;; (defn queue-count [agent]
;;   (-> agent .action-queue .get .queue .count))
