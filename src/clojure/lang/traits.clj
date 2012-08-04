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
      (set! watches (dissoc watches key)))))

(deftrait AValidable [^:volatile-mutable validator]
  IValidable
  (-set-validator! [this new-validator]
    (when (new-validator (-deref this))
      (set! validator new-validator)))
  (-get-validator [_] validator)
  (-validate [_ new-value]
    (when validator
      (assert (validator new-value) "Validator rejected reference state"))))

(defn throw-arity [this n]
  (let [name (-> this .getClass .getSimpleName)
        suffix (.lastIndexOf name "__")
        elided-name (if (= suffix -1) name (.substring name 0 suffix))]
    (throw (clojure.lang.ArityException. n (.replace elided-name \_ \-)))))

(defn gen-invoke [f]
  (map (fn [args] (list '-invoke (vec (cons 'this args))
                        (list f'this (count args))))
       (cons [] (take-while seq (iterate rest (repeat 18 '_))))))

(def AFn
  (list
   'java.lang.Callable
   '(call [this]
     (try (-invoke this)
       (catch Exception e
         (throw e))))
   'java.lang.Runnable
   '(run [this] (-invoke this))
   'IFn
   (gen-invoke `throw-arity)
   '(-apply [this arglist]
      (let [arglist-len (count arglist)] 
       (if (< arglist-len 19)
         (eval `(-invoke ~this ~@arglist))
         (eval `(-invoke ~this ~@(take 19 arglist) '~(drop 19 arglist))))))))
