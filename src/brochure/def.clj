(ns brochure.def
  (:refer-clojure :exclude [deftype]))

(defn merge-methods [[defaults provideds]]
  (let [explicitize (fn [[fname args & body]]
                      [[(meta fname) fname (list (meta args) (map meta args)) args] body])
        implicitize (fn [[[_ name _ args] body]]
                      `(~name ~args ~@body))
        defaults (apply hash-map (mapcat explicitize defaults))
        provideds (apply hash-map (mapcat explicitize provideds))
        methods (merge defaults provideds)]
    (map implicitize methods)))

(defn mismatching-mutable? [k1 k2]
  (not (apply =  
              (map (comp (juxt :unsynchronized-mutable
                               :volatile-mutable) meta) [k1 k2]))))

(defn validate-args [provided required]
  (let [pr (set provided)
        req (set required)
        missing (remove pr req)]
    (if-not (empty? missing)
      `(throw (Exception. (str "deftype declaration is missing the following args: " ~@(map str missing)
                               ", required by one ")))
      (if (some true? (map mismatching-mutable? (filter req pr) req))
        `(throw (Exception. "mutable declaration mismatching for one or more args"))))))

(defmacro deftype [name args & body]
  (if (= :defaults (first body))
    (let [body (next body)
          [defaults abstracts] ((juxt (comp (partial mapcat identity) filter) remove)
                                coll? (map eval (first body)))
          [required defaults] ((juxt (comp (partial mapcat identity) filter) remove)
                               vector? defaults)]
      (if-let [err (validate-args args required)] 
        err
        (if (empty? abstracts)
          (let [[methods protocols] ((juxt (partial map first)
                                           (comp set (partial mapcat second)))
                                     (map (partial (juxt filter remove) list?)
                                          [defaults (rest body)]))]
            `(clojure.core/deftype ~name ~args
               ~@protocols
               ~@(merge-methods methods)))
          `(throw (Exception. "not yet implemented :(")))))
    `(clojure.core/deftype ~name ~args ~@body)))

(defmacro deftrait [name elems & body]
  (if (= :abstract (first body))
    `(throw (Exception. "not yet implemented :("))
    `(def ~name '(~elems ~@body))))
