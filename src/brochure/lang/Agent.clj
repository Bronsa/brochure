(ns brochure.lang.Agent
  (:require [brochure.lang.protocols :refer :all]
            [brochure.lang.implementations :refer :all])
  (:import java.util.concurrent.atomic.AtomicLong
           java.util.concurrent.Executors
           java.util.concurrent.ThreadFactory
           java.util.concurrent.atomic.AtomicReference))

(deftype ActionQueue [q error]
  IEmptyableCollection
  (-empty [_] (ActionQueue. clojure.lang.PersistentQueue/EMPTY nil)))

(def send-thread-pool-counter (AtomicLong. 0))
(def send-off-thread-pool-counter (AtomicLong. 0))

(defn ^:private thread-factory [format-string counter]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable)
        (.setName (format format-string (.getAndIncrement counter)))))))

(def pooled-executor (Executors/newFixedThreadPool
                      (+ 2 (.availableProcessors (Runtime/getRuntime)))
                      (thread-factory "clojure-agent-send-pool-%d" send-thread-pool-counter)))

(def solo-executor (Executors/newCachedThreadPool
                    (thread-factory "clojure-agent-send-off-pool-%d" send-off-thread-pool-counter)))

(def nested (ThreadLocal.))

(defn shutdown []
  (.shutdown solo-executor)
  (.shutdown pooled-executor))

(defprotocol Executable (execute [this]))

(deftype Action [agent fn args solo?]

  Runnable
  (run [this]
    (try
      (.set nested [])
      (let [error (atom nil)]
        (try (let [old-value (-deref agent)
                   new-value (apply fn (-deref agent) args)]
               (-set! agent new-value)
               (-notify-watches agent old-value new-value))
         (catch Throwable e
           (reset! error e)))
        (if-not @error
          (release-pending-sends)
          (do (.set nested nil)
              (if-let [error-handler (.error-handler agent)]
                (try (error-handler agent error)
                     (catch Throwable _)))
              (if (#{:continue} (.error-mode agent))
                (reset! error nil))))
        (let [next (loop [pooped false next nil]
                     (if-not pooped
                       (let [prior (.get (.aq agent))
                             next (ActionQueue. (.pop (.q prior)) @error)]
                         (recur (.compareAndSet (.aq agent) prior next) next))
                       next))]
          (when-not (and error (> 0 (count (.q next))))
            (-> next .q .peek .execute))))
     (finally
       (.set nested nil))))

  Executable
  (execute [this]
    (try (.execute (if solo? solo-executor pooled-executor) this)
      (catch Throwable error
        (if-let [error-handler (.error-handler agent)]
          (try (error-handler agent error)
            (catch Throwable _)))))))

(defn dispatch-action [action]
  (let [trans (clojure.lang.LockingTransaction/getRunning)]
    (cond
      trans         (.enqueue trans action)
      (.get nested) (.set nested (-> nested .get (cons action)))
      :else         (.enqueue (.agent action) action))))

(defn release-pending-sends []
  (let [sends (.get nested)]
    (if-not sends
      0
      (do (doseq [a sends]
            (.enqueue (.agent a) a))
          (.set nested [])
          (count sends)))))

(deftype Agent [^:volatile-mutable state
                ^AtomicReference aq ;;AtomicRefernce<ActionQueue> EMPTY in constructor
                ^:volatile-mutable error-mode ;; :continue
                ^:volatile-mutable error-handler ;; nil
                ^:unsynchronized-mutable meta
                ^:volatile-mutable validator
                ^:volatile-mutable watches]
  :mixin (merge ARef IEquivImpl IHashImpl))
