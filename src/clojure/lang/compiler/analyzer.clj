;; Copyright (c) Rich Hickey. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns clojure.lang.compiler.analyzer
  (:refer-clojure :exclude [macroexpand-1 *ns* the-ns])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.lang.runtime :refer [*ns*]]
            [clojure.lang.ns :refer [namespaces the-ns ns-alias]]))

(do
  (in-ns 'clojure.core)
  (def ^:dynamic *warn-on-undeclared* false)
  (in-ns 'clojure.lang.compiler.analyzer)
  (refer 'clojure.core :only '[*warn-on-undeclared*]))

(defn empty-env []
  {:ns (the-ns *ns*) :context :statement :locals {}})

(defmacro ^:private debug-prn
  [& args]
  `(binding [*out* *err*]
     (println (str ~@args))))

(defn warning [env & s]
  (debug-prn
   "WARNING: " (apply str s)
   (when (:line env)
     (str " at line " (:line env) " " *file*))))

(defn confirm-var-exists [env ns sym]
  (when *warn-on-undeclared*
    (let [crnt-ns (-> env :ns :name)]
      (when (= ns crnt-ns)
        (when-not ((ns-map crnt-ns) sym)
          (warning env "Use of undeclared Var " ns "/" sym))))))

(defn resolve-ns-alias [env name]
  (let [sym (symbol name)]
    (or (ns-alias (:ns env) sym)
        sym)))

(defn core-name?
  "Is sym visible from core in the current compilation namespace?"
  [env sym]
  (and ((ns-map 'clojure.core) sym)
       ((ns-map (-> env :ns)) sym)))

(defn resolve-var
  ([env sym] (resolve-var env sym false))
  ([env sym confirm?]
     (let [s (str sym)
           lb (-> env :locals sym)]
       (cond
         lb lb
         
         (namespace sym)
         (let [ns (namespace sym)
               full-ns (resolve-ns-alias env ns)
               name (symbol (name sym))]
           (when confirm? (confirm-var-exists env full-ns name))
           (merge ((ns-map full-ns) name)
                  {:name name
                   :ns full-ns}))
         
         
         ;; namespaces and classes, needs work, currently not working
         ;; (.contains s ".")
         ;; (let [idx (.indexOf s ".")
         ;;       prefix (symbol (subs s 0 idx))
         ;;       suffix (subs s (inc idx))
         ;;       lb (-> env :locals prefix)]
         ;;   (if lb
         ;;     {:name (symbol (str (:name lb) suffix))}
         ;;     (do
         ;;       (when confirm? (confirm-var-exists env prefix (symbol suffix)))
         ;;       (merge (get-in @namespaces [prefix :defs (symbol suffix)])
         ;;              {:name (symbol (str prefix) suffix)
         ;;               :ns prefix}))))
         
         ;; (get-in @namespaces [(-> env :ns :name) :uses sym])
         ;; (let [full-ns (get-in @namespaces [(-> env :ns :name) :uses sym])]
         ;;   (merge
         ;;    (when confirm? (get-in @namespaces [full-ns :defs sym]))
         ;;    {:name (symbol (str full-ns) (str sym))
         ;;     :ns (-> env :ns :name)}))

         ;;   :else
         ;;   (let [full-ns (if (core-name? env sym)
         ;;                   'clojure.core
         ;;                   (-> env :ns :name))]
         ;;     (when confirm? (confirm-var-exists env full-ns sym))
         ;;     (merge ((ns-map full-ns) sym)
         ;;            {:name sym
         ;;             :ns full-ns}))
         ))))

(defn resolve-existing-var [env sym]
  (resolve-var env sym true))

(defn confirm-bindings [env names]
  (doseq [name names]
    (let [env (merge env {:ns (the-ns *ns*)})
          ev (resolve-existing-var env name)]
      (when (not (-> ev :meta :dynamic))
        (warning env
          (str (:name ev) " not declared ^:dynamic"))))))
 
;; (def specials '#{if def fn* do let* loop* letfn* throw try* recur new set! ns deftype* . & quote})

;; (def ^:dynamic *recur-frames* nil)
;; (def ^:dynamic *loop-lets* nil)

;; (defmacro disallowing-recur [& body]
;;   `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body))

;; (defn analyze-block
;;   "returns {:statements .. :ret ..}"
;;   [env exprs]
;;   (let [statements (disallowing-recur
;;                     (seq (map #(analyze (assoc env :context :statement) %) (butlast exprs))))
;;         ret (if (<= (count exprs) 1)
;;               (analyze env (first exprs))
;;               (analyze (assoc env :context (if (= :statement (:context env)) :statement :return))
;;                        (last exprs)))]
;;     {:statements statements :ret ret}))

;; (defn parse [op env form name]
;;   ((case op
;;      if parse-if)
;;    enf form name))

;; (defn parse-if
;;   [op env [_ test then else :as form] name]
;;   (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
;;         then-expr (analyze env then)
;;         else-expr (analyze env else)]
;;     {:env env :op :if :form form
;;      :test test-expr :then then-expr :else else-expr}))

;; (defn parse-throw
;;   [op env [_ throw :as form] name]
;;   (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw))]
;;     {:env env :op :throw :form form
;;      :throw throw-expr}))

;; (defn- block-children [{:keys [statements ret] :as block}]
;;   (when block (conj (vec statements) ret)))

;; (defn parse-try
;;   [op env [_ & body :as form] name]
;;   (let [body (vec body)
;;         catchenv (update-in env [:context] #(if (= :expr %) :return %))
;;         tail (peek body)
;;         fblock (when (and (seq? tail) (= 'finally (first tail)))
;;                   (rest tail))
;;         finally (when fblock
;;                   (analyze-block
;;                    (assoc env :context :statement)
;;                    fblock))
;;         body (if finally (pop body) body)
;;         tail (peek body)
;;         cblock (when (and (seq? tail)
;;                           (= 'catch (first tail)))
;;                  (rest tail))
;;         name (first cblock)
;;         locals (:locals catchenv)
;;         locals (if name
;;                  (assoc locals name {:name name})
;;                  locals)
;;         catch (when cblock
;;                 (analyze-block (assoc catchenv :locals locals) (rest cblock)))
;;         body (if name (pop body) body)
;;         try (when body
;;               (analyze-block (if (or name finally) catchenv env) body))]
;;     (when name (assert (not (namespace name)) "Can't qualify symbol in catch"))
;;     {:env env :op :try* :form form
;;      :try try
;;      :finally finally
;;      :name name
;;      :catch catch
;;      :children (vec (mapcat block-children
;;                             [try catch finally]))}))

;; (defmethod parse 'def
;;   [op env form name]
;;   (let [pfn (fn
;;               ([_ sym] {:sym sym})
;;               ([_ sym init] {:sym sym :init init})
;;               ([_ sym doc init] {:sym sym :doc doc :init init}))
;;         args (apply pfn form)
;;         sym (:sym args)
;;         tag (-> sym meta :tag)
;;         protocol (-> sym meta :protocol)
;;         dynamic (-> sym meta :dynamic)
;;         ns-name (-> env :ns :name)]
;;     (assert (not (namespace sym)) "Can't def ns-qualified name")
;;     (let [env (if (or (and (not= ns-name 'cljs.core)
;;                            (core-name? env sym))
;;                       (get-in @namespaces [ns-name :uses sym]))
;;                 (let [ev (resolve-existing-var (dissoc env :locals) sym)]
;;                   (when *cljs-warn-on-redef*
;;                     (warning env
;;                       (str sym " already refers to: " (:name ev)
;;                            " being replaced by: " (symbol (str ns-name) (str sym)))))
;;                   (swap! namespaces update-in [ns-name :excludes] conj sym)
;;                   (update-in env [:ns :excludes] conj sym))
;;                 env)
;;           name (:name (resolve-var (dissoc env :locals) sym))
;;           init-expr (when (contains? args :init)
;;                       (disallowing-recur
;;                        (analyze (assoc env :context :expr) (:init args) sym)))
;;           fn-var? (and init-expr (= (:op init-expr) :fn))
;;           export-as (when-let [export-val (-> sym meta :export)]
;;                       (if (= true export-val) name export-val))
;;           doc (or (:doc args) (-> sym meta :doc))]
;;       (when-let [v (get-in @namespaces [ns-name :defs sym])]
;;         (when (and *cljs-warn-on-fn-var*
;;                    (not (-> sym meta :declared))
;;                    (and (:fn-var v) (not fn-var?)))
;;           (warning env
;;             (str (symbol (str ns-name) (str sym))
;;                  " no longer fn, references are stale"))))
;;       (swap! namespaces update-in [ns-name :defs sym]
;;              (fn [m]
;;                (let [m (assoc (or m {}) :name name)]
;;                  (merge m
;;                    (when tag {:tag tag})
;;                    (when dynamic {:dynamic true})
;;                    (when-let [line (:line env)]
;;                      {:file *cljs-file* :line line})
;;                    ;; the protocol a protocol fn belongs to
;;                    (when protocol
;;                      {:protocol protocol})
;;                    ;; symbol for reified protocol
;;                    (when-let [protocol-symbol (-> sym meta :protocol-symbol)]
;;                      {:protocol-symbol protocol-symbol})
;;                    (when fn-var?
;;                      {:fn-var true
;;                       ;; protocol implementation context
;;                       :protocol-impl (:protocol-impl init-expr)
;;                       ;; inline protocol implementation context
;;                       :protocol-inline (:protocol-inline init-expr)
;;                       :variadic (:variadic init-expr)
;;                       :max-fixed-arity (:max-fixed-arity init-expr)
;;                       :method-params (map (fn [m]
;;                                             (:params m))
;;                                           (:methods init-expr))})))))
;;       (merge {:env env :op :def :form form
;;               :name name :doc doc :init init-expr}
;;              (when tag {:tag tag})
;;              (when dynamic {:dynamic true})
;;              (when export-as {:export export-as})
;;              (when init-expr {:children [init-expr]})))))

;; (defn- analyze-fn-method [env locals meth gthis]
;;   (letfn [(uniqify [[p & r]]
;;             (when p
;;               (cons (if (some #{p} r) (gensym (str p)) p)
;;                     (uniqify r))))]
;;    (let [params (first meth)
;;          variadic (boolean (some '#{&} params))
;;          params (vec (uniqify (remove '#{&} params)))
;;          fixed-arity (count (if variadic (butlast params) params))
;;          body (next meth)
;;          locals (reduce (fn [m name]
;;                           (assoc m name {:name name
;;                                          :tag (-> name meta :tag)}))
;;                         locals params)
;;          recur-frame {:names params :flag (atom nil)}
;;          block (binding [*recur-frames* (cons recur-frame *recur-frames*)]
;;                  (analyze-block (assoc env :context :return :locals locals) body))]
;;      (merge {:env env :variadic variadic :params params :max-fixed-arity fixed-arity
;;              :gthis gthis :recurs @(:flag recur-frame)}
;;             block))))

;; (defmethod parse 'fn*
;;   [op env [_ & args :as form] name]
;;   (let [[name meths] (if (symbol? (first args))
;;                        [(first args) (next args)]
;;                        [name (seq args)])
;;         ;;turn (fn [] ...) into (fn ([]...))
;;         meths (if (vector? (first meths)) (list meths) meths)
;;         locals (:locals env)
;;         locals (if name (assoc locals name {:name name}) locals)
;;         fields (-> form meta ::fields)
;;         protocol-impl (-> form meta :protocol-impl)
;;         protocol-inline (-> form meta :protocol-inline)
;;         gthis (and fields (gensym "this__"))
;;         locals (reduce (fn [m fld]
;;                          (assoc m fld
;;                                 {:name (symbol (str gthis "." fld))
;;                                  :field true
;;                                  :mutable (-> fld meta :mutable)
;;                                  :tag (-> fld meta :tag)}))
;;                        locals fields)

;;         menv (if (> (count meths) 1) (assoc env :context :expr) env)
;;         menv (merge menv
;;                {:protocol-impl protocol-impl
;;                 :protocol-inline protocol-inline})
;;         methods (map #(analyze-fn-method menv locals % gthis) meths)
;;         max-fixed-arity (apply max (map :max-fixed-arity methods))
;;         variadic (boolean (some :variadic methods))
;;         locals (if name (assoc locals name {:name name :fn-var true
;;                                             :variadic variadic
;;                                             :max-fixed-arity max-fixed-arity
;;                                             :method-params (map :params methods)}))
;;         methods (if name
;;                   ;; a second pass with knowledge of our function-ness/arity
;;                   ;; lets us optimize self calls
;;                   (map #(analyze-fn-method menv locals % gthis) meths)
;;                   methods)]
;;     ;;todo - validate unique arities, at most one variadic, variadic takes max required args
;;     {:env env :op :fn :form form :name name :methods methods :variadic variadic
;;      :recur-frames *recur-frames* :loop-lets *loop-lets*
;;      :jsdoc [(when variadic "@param {...*} var_args")]
;;      :max-fixed-arity max-fixed-arity
;;      :protocol-impl protocol-impl
;;      :protocol-inline protocol-inline
;;      :children (vec (mapcat block-children
;;                             methods))}))

;; (defmethod parse 'letfn*
;;   [op env [_ bindings & exprs :as form] name]
;;   (assert (and (vector? bindings) (even? (count bindings))) "bindings must be vector of even number of elements")
;;   (let [n->fexpr (into {} (map (juxt first second) (partition 2 bindings)))
;;         names    (keys n->fexpr)
;;         n->gsym  (into {} (map (juxt identity #(gensym (str % "__"))) names))
;;         gsym->n  (into {} (map (juxt n->gsym identity) names))
;;         context  (:context env)
;;         bes      (reduce (fn [bes n]
;;                            (let [g (n->gsym n)]
;;                              (conj bes {:name  g
;;                                         :tag   (-> n meta :tag)
;;                                         :local true})))
;;                          []
;;                          names)
;;         meth-env (reduce (fn [env be]
;;                            (let [n (gsym->n (be :name))]
;;                              (assoc-in env [:locals n] be)))
;;                          (assoc env :context :expr)
;;                          bes)
;;         [meth-env finits]
;;         (reduce (fn [[env finits] n]
;;                   (let [finit (analyze meth-env (n->fexpr n))
;;                         be (-> (get-in env [:locals n])
;;                                (assoc :init finit))]
;;                     [(assoc-in env [:locals n] be)
;;                      (conj finits finit)]))
;;                 [meth-env []]
;;                 names)
;;         {:keys [statements ret]}
;;         (analyze-block (assoc meth-env :context (if (= :expr context) :return context)) exprs)
;;         bes (vec (map #(get-in meth-env [:locals %]) names))]
;;     {:env env :op :letfn :bindings bes :statements statements :ret ret :form form
;;      :children (into (vec (map :init bes))
;;                      (conj (vec statements) ret))}))

;; (defmethod parse 'do
;;   [op env [_ & exprs :as form] _]
;;   (let [block (analyze-block env exprs)]
;;     (merge {:env env :op :do :form form :children (block-children block)} block)))

;; (defn analyze-let
;;   [encl-env [_ bindings & exprs :as form] is-loop]
;;   (assert (and (vector? bindings) (even? (count bindings))) "bindings must be vector of even number of elements")
;;   (let [context (:context encl-env)
;;         [bes env]
;;         (disallowing-recur
;;          (loop [bes []
;;                 env (assoc encl-env :context :expr)
;;                 bindings (seq (partition 2 bindings))]
;;            (if-let [[name init] (first bindings)]
;;              (do
;;                (assert (not (or (namespace name) (.contains (str name) "."))) (str "Invalid local name: " name))
;;                (let [init-expr (analyze env init)
;;                      be {:name (gensym (str name "__"))
;;                          :init init-expr
;;                          :tag (or (-> name meta :tag)
;;                                   (-> init-expr :tag)
;;                                   (-> init-expr :info :tag))
;;                          :local true}]
;;                  (recur (conj bes be)
;;                         (assoc-in env [:locals name] be)
;;                         (next bindings))))
;;              [bes env])))
;;         recur-frame (when is-loop {:names (vec (map :name bes)) :flag (atom nil)})
;;         {:keys [statements ret]}
;;         (binding [*recur-frames* (if recur-frame (cons recur-frame *recur-frames*) *recur-frames*)
;;                   *loop-lets* (cond
;;                                is-loop (or *loop-lets* ())
;;                                *loop-lets* (cons {:names (vec (map :name bes))} *loop-lets*))]
;;           (analyze-block (assoc env :context (if (= :expr context) :return context)) exprs))]
;;     {:env encl-env :op :let :loop is-loop
;;      :bindings bes :statements statements :ret ret :form form
;;      :children (into (vec (map :init bes))
;;                      (conj (vec statements) ret))}))

;; (defmethod parse 'let*
;;   [op encl-env form _]
;;   (analyze-let encl-env form false))

;; (defmethod parse 'loop*
;;   [op encl-env form _]
;;   (analyze-let encl-env form true))

;; (defmethod parse 'recur
;;   [op env [_ & exprs :as form] _]
;;   (let [context (:context env)
;;         frame (first *recur-frames*)
;;         exprs (disallowing-recur (vec (map #(analyze (assoc env :context :expr) %) exprs)))]
;;     (assert frame "Can't recur here")
;;     (assert (= (count exprs) (count (:names frame))) "recur argument count mismatch")
;;     (reset! (:flag frame) true)
;;     (assoc {:env env :op :recur :form form}
;;       :frame frame
;;       :exprs exprs
;;       :children exprs)))

;; (defmethod parse 'quote
;;   [_ env [_ x] _]
;;   {:op :constant :env env :form x})

;; (defmethod parse 'new
;;   [_ env [_ ctor & args :as form] _]
;;   (disallowing-recur
;;    (let [enve (assoc env :context :expr)
;;          ctorexpr (analyze enve ctor)
;;          argexprs (vec (map #(analyze enve %) args))
;;          known-num-fields (:num-fields (resolve-existing-var env ctor))
;;          argc (count args)]
;;      (when (and known-num-fields (not= known-num-fields argc))
;;        (warning env
;;          (str "Wrong number of args (" argc ") passed to " ctor)))
     
;;      {:env env :op :new :form form :ctor ctorexpr :args argexprs
;;       :children (into [ctorexpr] argexprs)})))

;; (defmethod parse 'set!
;;   [_ env [_ target val alt :as form] _]
;;   (let [[target val] (if alt
;;                        ;; (set! o -prop val)
;;                        [`(. ~target ~val) alt]
;;                        [target val])]
;;     (disallowing-recur
;;      (let [enve (assoc env :context :expr)
;;            targetexpr (cond
;;                        ;; TODO: proper resolve
;;                        (= target '*unchecked-if*)
;;                        (do
;;                          (reset! *unchecked-if* val)
;;                          ::set-unchecked-if)

;;                        (symbol? target)
;;                        (do
;;                          (let [local (-> env :locals target)]
;;                            (assert (or (nil? local)
;;                                        (and (:field local)
;;                                             (:mutable local)))
;;                                    "Can't set! local var or non-mutable field"))
;;                          (analyze-symbol enve target))

;;                        :else
;;                        (when (seq? target)
;;                          (let [targetexpr (analyze-seq enve target nil)]
;;                            (when (:field targetexpr)
;;                              targetexpr))))
;;            valexpr (analyze enve val)]
;;        (assert targetexpr "set! target must be a field or a symbol naming a var")
;;        (cond
;;         (= targetexpr ::set-unchecked-if) {:env env :op :no-op}
;;         :else {:env env :op :set! :form form :target targetexpr :val valexpr
;;                :children [targetexpr valexpr]})))))

;; (defn munge-path [ss]
;;   (clojure.lang.Compiler/munge (str ss)))

;; (defn ns->relpath [s]
;;   (str (string/replace (munge-path s) \. \/) ".cljs"))

;; (declare analyze-file)

;; (defn analyze-deps [deps]
;;   (doseq [dep deps]
;;     (when-not (:defs (@namespaces dep))
;;       (let [relpath (ns->relpath dep)]
;;         (when (io/resource relpath)
;;           (analyze-file relpath))))))

;; (defmethod parse 'ns
;;   [_ env [_ name & args :as form] _]
;;   (let [docstring (if (string? (first args)) (first args) nil)
;;         args      (if docstring (next args) args)
;;         excludes
;;         (reduce (fn [s [k exclude xs]]
;;                   (if (= k :refer-clojure)
;;                     (do
;;                       (assert (= exclude :exclude) "Only [:refer-clojure :exclude [names]] form supported")
;;                       (assert (not (seq s)) "Only one :refer-clojure form is allowed per namespace definition")
;;                       (into s xs))
;;                     s))
;;                 #{} args)
;;         deps (atom #{})
;;         valid-forms (atom #{:use :use-macros :require :require-macros})
;;         error-msg (fn [spec msg] (str msg "; offending spec: " (pr-str spec)))
;;         parse-require-spec (fn parse-require-spec [macros? spec]
;;                              (assert (or (symbol? spec) (vector? spec))
;;                                      (error-msg spec "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros"))
;;                              (when (vector? spec)
;;                                (assert (symbol? (first spec))
;;                                        (error-msg spec "Library name must be specified as a symbol in :require / :require-macros"))
;;                                (assert (odd? (count spec))
;;                                        (error-msg spec "Only :as alias and :refer [names] options supported in :require"))
;;                                (assert (every? #{:as :refer} (map first (partition 2 (next spec))))
;;                                        (error-msg spec "Only :as and :refer options supported in :require / :require-macros"))
;;                                (assert (let [fs (frequencies (next spec))]
;;                                          (and (<= (fs :as 0) 1)
;;                                               (<= (fs :refer 0) 1)))
;;                                        (error-msg spec "Each of :as and :refer options may only be specified once in :require / :require-macros")))
;;                              (if (symbol? spec)
;;                                (recur macros? [spec])
;;                                (let [[lib & opts] spec
;;                                      {alias :as referred :refer :or {alias lib}} (apply hash-map opts)
;;                                      [rk uk] (if macros? [:require-macros :use-macros] [:require :use])]
;;                                  (assert (or (symbol? alias) (nil? alias))
;;                                          (error-msg spec ":as must be followed by a symbol in :require / :require-macros"))
;;                                  (assert (or (and (vector? referred) (every? symbol? referred))
;;                                              (nil? referred))
;;                                          (error-msg spec ":refer must be followed by a vector of symbols in :require / :require-macros"))
;;                                  (swap! deps conj lib)
;;                                  (merge (when alias {rk {alias lib}})
;;                                         (when referred {uk (apply hash-map (interleave referred (repeat lib)))})))))
;;         use->require (fn use->require [[lib kw referred :as spec]]
;;                        (assert (and (symbol? lib) (= :only kw) (vector? referred) (every? symbol? referred))
;;                                (error-msg spec "Only [lib.ns :only [names]] specs supported in :use / :use-macros"))
;;                        [lib :refer referred])
;;         {uses :use requires :require uses-macros :use-macros requires-macros :require-macros :as params}
;;         (reduce (fn [m [k & libs]]
;;                   (assert (#{:use :use-macros :require :require-macros} k)
;;                           "Only :refer-clojure, :require, :require-macros, :use and :use-macros libspecs supported")
;;                   (assert (@valid-forms k)
;;                           (str "Only one " k " form is allowed per namespace definition"))
;;                   (swap! valid-forms disj k)
;;                   (apply merge-with merge m
;;                          (map (partial parse-require-spec (contains? #{:require-macros :use-macros} k))
;;                               (if (contains? #{:use :use-macros} k)
;;                                 (map use->require libs)
;;                                 libs))))
;;                 {} (remove (fn [[r]] (= r :refer-clojure)) args))]
;;     (when (seq @deps)
;;       (analyze-deps @deps))
;;     (set! *cljs-ns* name)
;;     (load-core)
;;     (doseq [nsym (concat (vals requires-macros) (vals uses-macros))]
;;       (clojure.core/require nsym))
;;     (swap! namespaces #(-> %
;;                            (assoc-in [name :name] name)
;;                            (assoc-in [name :excludes] excludes)
;;                            (assoc-in [name :uses] uses)
;;                            (assoc-in [name :requires] requires)
;;                            (assoc-in [name :uses-macros] uses-macros)
;;                            (assoc-in [name :requires-macros]
;;                                      (into {} (map (fn [[alias nsym]]
;;                                                      [alias (find-ns nsym)])
;;                                                    requires-macros)))))
;;     {:env env :op :ns :form form :name name :uses uses :requires requires
;;      :uses-macros uses-macros :requires-macros requires-macros :excludes excludes}))

;; (defmethod parse 'deftype*
;;   [_ env [_ tsym fields pmasks :as form] _]
;;   (let [t (:name (resolve-var (dissoc env :locals) tsym))]
;;     (swap! namespaces update-in [(-> env :ns :name) :defs tsym]
;;            (fn [m]
;;              (let [m (assoc (or m {})
;;                        :name t
;;                        :type true
;;                        :num-fields (count fields))]
;;                (merge m
;;                  {:protocols (-> tsym meta :protocols)}     
;;                  (when-let [line (:line env)]
;;                    {:file *cljs-file*
;;                     :line line})))))
;;     {:env env :op :deftype* :form form :t t :fields fields :pmasks pmasks}))

;; (defmethod parse 'defrecord*
;;   [_ env [_ tsym fields pmasks :as form] _]
;;   (let [t (:name (resolve-var (dissoc env :locals) tsym))]
;;     (swap! namespaces update-in [(-> env :ns :name) :defs tsym]
;;            (fn [m]
;;              (let [m (assoc (or m {}) :name t :type true)]
;;                (merge m
;;                  {:protocols (-> tsym meta :protocols)}
;;                  (when-let [line (:line env)]
;;                    {:file *cljs-file*
;;                     :line line})))))
;;     {:env env :op :defrecord* :form form :t t :fields fields :pmasks pmasks}))

;; ;; dot accessor code

;; (def ^:private property-symbol? #(boolean (and (symbol? %) (re-matches #"^-.*" (name %)))))

;; (defn- classify-dot-form
;;   [[target member args]]
;;   [(cond (nil? target) ::error
;;          :default      ::expr)
;;    (cond (property-symbol? member) ::property
;;          (symbol? member)          ::symbol
;;          (seq? member)             ::list
;;          :default                  ::error)
;;    (cond (nil? args) ()
;;          :default    ::expr)])

;; (defmulti build-dot-form #(classify-dot-form %))

;; ;; (. o -p)
;; ;; (. (...) -p)
;; (defmethod build-dot-form [::expr ::property ()]
;;   [[target prop _]]
;;   {:dot-action ::access :target target :field (-> prop name (.substring 1) symbol)})

;; ;; (. o -p <args>)
;; (defmethod build-dot-form [::expr ::property ::list]
;;   [[target prop args]]
;;   (throw (Error. (str "Cannot provide arguments " args " on property access " prop))))

;; (defn- build-method-call
;;   "Builds the intermediate method call map used to reason about the parsed form during
;;   compilation."
;;   [target meth args]
;;   (if (symbol? meth)
;;     {:dot-action ::call :target target :method meth :args args}
;;     {:dot-action ::call :target target :method (first meth) :args args}))

;; ;; (. o m 1 2)
;; (defmethod build-dot-form [::expr ::symbol ::expr]
;;   [[target meth args]]
;;   (build-method-call target meth args))

;; ;; (. o m)
;; (defmethod build-dot-form [::expr ::symbol ()]
;;   [[target meth args]]
;;   (build-method-call target meth args))

;; ;; (. o (m))
;; ;; (. o (m 1 2))
;; (defmethod build-dot-form [::expr ::list ()]
;;   [[target meth-expr _]]
;;   (build-method-call target (first meth-expr) (rest meth-expr)))

;; (defmethod build-dot-form :default
;;   [dot-form]
;;   (throw (Error. (str "Unknown dot form of " (list* '. dot-form) " with classification " (classify-dot-form dot-form)))))

;; (defmethod parse '.
;;   [_ env [_ target & [field & member+] :as form] _]
;;   (disallowing-recur
;;    (let [{:keys [dot-action target method field args]} (build-dot-form [target field member+])
;;          enve        (assoc env :context :expr)
;;          targetexpr  (analyze enve target)]
;;      (case dot-action
;;            ::access {:env env :op :dot :form form
;;                      :target targetexpr
;;                      :field field
;;                      :children [targetexpr]
;;                      :tag (-> form meta :tag)}
;;            ::call   (let [argexprs (map #(analyze enve %) args)]
;;                       {:env env :op :dot :form form
;;                        :target targetexpr
;;                        :method method
;;                        :args argexprs
;;                        :children (into [targetexpr] argexprs)
;;                        :tag (-> form meta :tag)})))))

;; (defmethod parse 'js*
;;   [op env [_ jsform & args :as form] _]
;;   (assert (string? jsform))
;;   (if args
;;     (disallowing-recur
;;      (let [seg (fn seg [^String s]
;;                  (let [idx (.indexOf s "~{")]
;;                    (if (= -1 idx)
;;                      (list s)
;;                      (let [end (.indexOf s "}" idx)]
;;                        (cons (subs s 0 idx) (seg (subs s (inc end))))))))
;;            enve (assoc env :context :expr)
;;            argexprs (vec (map #(analyze enve %) args))]
;;        {:env env :op :js :segs (seg jsform) :args argexprs
;;         :tag (-> form meta :tag) :form form :children argexprs}))
;;     (let [interp (fn interp [^String s]
;;                    (let [idx (.indexOf s "~{")]
;;                      (if (= -1 idx)
;;                        (list s)
;;                        (let [end (.indexOf s "}" idx)
;;                              inner (:name (resolve-existing-var env (symbol (subs s (+ 2 idx) end))))]
;;                          (cons (subs s 0 idx) (cons inner (interp (subs s (inc end)))))))))]
;;       {:env env :op :js :form form :code (apply str (interp jsform))
;;        :tag (-> form meta :tag)})))

;; (defn parse-invoke
;;   [env [f & args :as form]]
;;   (disallowing-recur
;;    (let [enve (assoc env :context :expr)
;;          fexpr (analyze enve f)
;;          argexprs (vec (map #(analyze enve %) args))
;;          argc (count args)]
;;      (if (and *cljs-warn-fn-arity* (-> fexpr :info :fn-var))
;;        (let [{:keys [variadic max-fixed-arity method-params name]} (:info fexpr)]
;;          (when (and (not (some #{argc} (map count method-params)))
;;                     (or (not variadic)
;;                         (and variadic (< argc max-fixed-arity))))
;;            (warning env
;;              (str "Wrong number of args (" argc ") passed to " name)))))
;;      {:env env :op :invoke :form form :f fexpr :args argexprs
;;       :tag (or (-> fexpr :info :tag) (-> form meta :tag)) :children (into [fexpr] argexprs)})))

;; (defn analyze-symbol
;;   "Finds the var associated with sym"
;;   [env sym]
;;   (let [ret {:env env :form sym}
;;         lb (-> env :locals sym)]
;;     (if lb
;;       (assoc ret :op :var :info lb)
;;       (assoc ret :op :var :info (resolve-existing-var env sym)))))

;; (defn get-expander [sym env]
;;   (let [mvar
;;         (when-not (or (-> env :locals sym)        ;locals hide macros
;;                       (and (or (-> env :ns :excludes sym)
;;                                (get-in @namespaces [(-> env :ns :name) :excludes sym]))
;;                            (not (or (-> env :ns :uses-macros sym)
;;                                     (get-in @namespaces [(-> env :ns :name) :uses-macros sym])))))
;;           (if-let [nstr (namespace sym)]
;;             (when-let [ns (cond
;;                            (= "clojure.core" nstr) (find-ns 'cljs.core)
;;                            (.contains nstr ".") (find-ns (symbol nstr))
;;                            :else
;;                            (-> env :ns :requires-macros (get (symbol nstr))))]
;;               (.findInternedVar ^clojure.lang.Namespace ns (symbol (name sym))))
;;             (if-let [nsym (-> env :ns :uses-macros sym)]
;;               (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) sym)
;;               (.findInternedVar ^clojure.lang.Namespace (find-ns 'cljs.core) sym))))]
;;     (when (and mvar (.isMacro ^clojure.lang.Var mvar))
;;       @mvar)))

;; (defn macroexpand-1 [env form]
;;   (let [op (first form)]
;;     (if (specials op)
;;       form
;;       (if-let [mac (and (symbol? op) (get-expander op env))]
;;         (binding [*ns* (create-ns *cljs-ns*)]
;;           (apply mac form env (rest form)))
;;         (if (symbol? op)
;;           (let [opname (str op)]
;;             (cond
;;              (= (first opname) \.) (let [[target & args] (next form)]
;;                                      (with-meta (list* '. target (symbol (subs opname 1)) args)
;;                                        (meta form)))
;;              (= (last opname) \.) (with-meta
;;                                     (list* 'new (symbol (subs opname 0 (dec (count opname)))) (next form))
;;                                     (meta form))
;;              :else form))
;;           form)))))

;; (defn analyze-seq
;;   [env form name]
;;   (let [env (assoc env :line
;;                    (or (-> form meta :line)
;;                        (:line env)))]
;;     (let [op (first form)]
;;       (assert (not (nil? op)) "Can't call nil")
;;       (let [mform (macroexpand-1 env form)]
;;         (if (identical? form mform)
;;           (if (specials op)
;;             (parse op env form name)
;;             (parse-invoke env form))
;;           (analyze env mform name))))))

;; (declare analyze-wrap-meta)

;; (defn analyze-map
;;   [env form name]
;;   (let [expr-env (assoc env :context :expr)
;;         simple-keys? (every? #(or (string? %) (keyword? %))
;;                              (keys form))
;;         ks (disallowing-recur (vec (map #(analyze expr-env % name) (keys form))))
;;         vs (disallowing-recur (vec (map #(analyze expr-env % name) (vals form))))]
;;     (analyze-wrap-meta {:op :map :env env :form form
;;                         :keys ks :vals vs :simple-keys? simple-keys?
;;                         :children (vec (interleave ks vs))}
;;                        name)))

;; (defn analyze-vector
;;   [env form name]
;;   (let [expr-env (assoc env :context :expr)
;;         items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
;;     (analyze-wrap-meta {:op :vector :env env :form form :items items :children items} name)))

;; (defn analyze-set
;;   [env form name]
;;   (let [expr-env (assoc env :context :expr)
;;         items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
;;     (analyze-wrap-meta {:op :set :env env :form form :items items :children items} name)))

;; (defn analyze-wrap-meta [expr name]
;;   (let [form (:form expr)]
;;     (if (meta form)
;;       (let [env (:env expr) ; take on expr's context ourselves
;;             expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
;;             meta-expr (analyze-map (:env expr) (meta form) name)]
;;         {:op :meta :env env :form form
;;          :meta meta-expr :expr expr :children [meta-expr expr]})
;;       expr)))

;; (defn analyze
;;   "Given an environment, a map containing {:locals (mapping of names to bindings), :context
;;   (one of :statement, :expr, :return), :ns (a symbol naming the
;;   compilation ns)}, and form, returns an expression object (a map
;;   containing at least :form, :op and :env keys). If expr has any (immediately)
;;   nested exprs, must have :children [exprs...] entry. This will
;;   facilitate code walking without knowing the details of the op set."
;;   ([env form] (analyze env form nil))
;;   ([env form name]
;;      (let [form (if (instance? clojure.lang.LazySeq form)
;;                   (or (seq form) ())
;;                   form)]
;;        (load-core)
;;        (cond
;;         (symbol? form) (analyze-symbol env form)
;;         (and (seq? form) (seq form)) (analyze-seq env form name)
;;         (map? form) (analyze-map env form name)
;;         (vector? form) (analyze-vector env form name)
;;         (set? form) (analyze-set env form name)
;;         :else {:op :constant :env env :form form}))))

;; (defn analyze-file
;;   [f]
;;   (let [res (if (= \/ (first f)) f (io/resource f))]
;;     (assert res (str "Can't find " f " in classpath"))
;;     (binding [*cljs-ns* 'cljs.user
;;               *cljs-file* (.getPath ^java.net.URL res)
;;               *ns* *reader-ns*]
;;       (with-open [r (io/reader res)]
;;         (let [env (empty-env)
;;               pbr (clojure.lang.LineNumberingPushbackReader. r)
;;               eof (Object.)]
;;           (loop [r (read pbr false eof false)]
;;             (let [env (assoc env :ns (get-namespace *cljs-ns*))]
;;               (when-not (identical? eof r)
;;                 (analyze env r)
;;                 (recur (read pbr false eof false))))))))))

