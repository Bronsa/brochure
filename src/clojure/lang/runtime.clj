(ns clojure.lang.runtime
  (:refer-clojure :exclude [*ns*])
  (:require [clojure.lang.ns :as ns]))

(def *ns* ns/*ns*)
