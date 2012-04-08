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

(defprotocol ISettable
  (-set! [this val]))

(defprotocol IRestartable
  (-restart [this new-state clear?]))

(defprotocol IError
  (-error [this])
  (-error-mode [thi])
  (-error-mode! [this k])
  (-error-handler [this])
  (-error-handler! [this f]))

(deftype Agent [^:volatile-mutable state
                ^AtomicReference aq ;;AtomicRefernce<ActionQueue> EMPTY in constructor
                ^:volatile-mutable error-mode ;; :continue
                ^:volatile-mutable error-handler ;; nil
                ^:unsynchronized-mutable meta
                ^:volatile-mutable validator
                ^:volatile-mutable watches]
  :mixin (merge ARef IEquivImpl IHashImpl)

  ISettable
  (-set! [this new-state]
    (-validate this new-state)
    (when (not= state new-state)
      (set! state new-state)
      true))

  IDeref
  (-deref [_] state)

  IError
  (-error [_] (-> aq .get .error))
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
      (.set aq (-empty (ActionQueue. nil nil))) ;; derp
      (loop [restarted false prior nil]
        (if-not restarted
          (let [prior (.get aq)]
            (recur (.compareAndSet aq prior (ActionQueue. (.q prior) nil)) prior))
          (if (pos? (-> prior .q .count))
            (.execute ^Action (-> prior .q .peek))))))
    new-state))


;; public Object dispatch(IFn fn, ISeq args, boolean solo) {
;; 	Throwable error = getError();
;; 	if(error != null)
;; 		{
;; 		throw Util.runtimeException("Agent is failed, needs restart", error);
;; 		}
;; 	Action action = new Action(this, fn, args, solo);
;; 	dispatchAction(action);

;; 	return this;
;; }

;; static void dispatchAction(Action action){
;; 	LockingTransaction trans = LockingTransaction.getRunning();
;; 	if(trans != null)
;; 		trans.enqueue(action);
;; 	else if(nested.get() != null)
;; 		{
;; 		nested.set(nested.get().cons(action));
;; 		}
;; 	else
;; 		action.agent.enqueue(action);
;; }

;; void enqueue(Action action){
;; 	boolean queued = false;
;; 	ActionQueue prior = null;
;; 	while(!queued)
;; 		{
;; 		prior = aq.get();
;; 		queued = aq.compareAndSet(prior, new ActionQueue((IPersistentStack)prior.q.cons(action), prior.error));
;; 		}

;; 	if(prior.q.count() == 0 && prior.error == null)
;; 		action.execute();
;; }

;; public int getQueueCount(){
;; 	return aq.get().q.count();
;; }

;; static public int releasePendingSends(){
;; 	IPersistentVector sends = nested.get();
;; 	if(sends == null)
;; 		return 0;
;; 	for(int i=0;i<sends.count();i++)
;; 		{
;; 		Action a = (Action) sends.valAt(i);
;; 		a.agent.enqueue(a);
;; 		}
;; 	nested.set(PersistentVector.EMPTY);
;; 	return sends.count();
;; }
;; }

