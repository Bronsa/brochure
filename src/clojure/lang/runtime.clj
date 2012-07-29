(set! *warn-on-reflection* true)

(ns clojure.lang.runtime
  (:refer-clojure :exclude [*ns*]))

(defonce ^:dynamic *ns* 'user)

