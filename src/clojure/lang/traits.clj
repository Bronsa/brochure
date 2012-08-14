(set! *warn-on-reflection* true)

(ns clojure.lang.traits
  (:require [brochure.def :refer [deftrait]]
            [clojure.lang.protocols :refer :all]))

(deftrait AReference [^:unsynchronized-mutable meta]
  IMeta
  (-meta [this]
    (locking this meta))
  
  IResetMeta
  (-reset-meta! [this m]
    (locking this (set! meta m))))

(deftrait AWatchable [^:volatile-mutable watches]
  IWatchable
  (-notify-watches [this old-value new-value]
    (doseq [[key f] watches]
      (f key this old-value new-value)))
  (-add-watch [this key f]
    (locking this
      (set! watches (assoc watches key f))))
  (-remove-watch [this key]
    (locking this
      (set! watches (dissoc watches key))))
  (-watches [this] watches))

(deftrait AValidable [^:volatile-mutable validator]
  IValidable
  (-set-validator! [this new-validator]
    (when (new-validator (-deref this))
      (set! validator new-validator)))
  (-get-validator [_] validator)
  (-validate [_ new-value]
    (when validator
      (assert (validator new-value) "Validator rejected reference state"))))
;; (IllegalStateException. "Invalid reference state")

(defn throw-arity [^Object this & args]
  (let [n (count args)
        name (-> this .getClass .getSimpleName)
        suffix (.lastIndexOf name "__")
        elided-name (if (= suffix -1) name (.substring name 0 suffix))]
    (throw (clojure.lang.ArityException. n (.replace elided-name \_ \-)))))

(defn gen-invoke [f this]
  (map (fn [args] (list '-invoke (vec (cons 'this args))
                        (list* f this args)))
          (cons [] (take-while seq (iterate rest (repeat 18 '_))))))

(def AFn
  (cons []
        (into 
         (list
          'java.util.concurrent.Callable
          '(call [this] (-invoke this))
          'java.lang.Runnable
          '(run [this]
                (try (-invoke this)
                     (catch Exception e
                       (throw e))))
          'IFn
          '(-apply [this arglist]
                   (let [arglist-len (count arglist)] 
                     (if (< arglist-len 19)
                       (eval `(-invoke ~this ~@arglist))
                       (eval `(-invoke ~this ~@(take 19 arglist) '~(drop 19 arglist)))))))
         (gen-invoke `throw-arity 'this))))


(deftrait AVMutable [^:volatile-mutable value]
  IMutableField
  (set-field! [this new-val] (set! value new-val))
  (get-field [this] value))
