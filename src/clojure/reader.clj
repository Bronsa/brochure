;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.reader
  (:refer-clojure :exclude [read read-line read-string])
  (:import (clojure.lang BigInt Numbers PersistentHashMap PersistentHashSet IMeta ISeq
                         RT IReference Symbol IPersistentList Reflector Var Symbol Keyword IObj
                         PersistentVector IPersistentCollection IRecord)
           (java.util ArrayList regex.Pattern regex.Matcher)
           java.lang.reflect.Constructor))

(set! *warn-on-reflection* true)

(defprotocol PushbackReader
  (read-char [reader] "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char [reader] "Same as (let [c (read-char rdr)] (unread rdr c) c) but without the overhead of unread")
  (unread [reader ch] "Push back a single character on to the stream"))

(defmacro update! [what f]
  (list 'set! what (list f what)))

(deftype StringPushbackReader
    [^:unsynchronized-mutable ^String s ^:unsynchronized-mutable buf
     ^:unsynchronized-mutable len ^:unsynchronized-mutable buf?]
  PushbackReader
  (read-char [reader]
    (if buf?
      (do (set! buf? false)
          buf)
      (when (pos? len)
        (let [r (.charAt s 0)]
          (update! len dec)
          (set! s (.substring s 1))
          r))))
  (peek-char [reader]
    (if buf?
      buf
      (when (pos? len)
          (.charAt s 0))))
  (unread [reader ch]
    (when ch
      (if buf? (throw (RuntimeException. "Pushback buffer is full")))
      (set! buf ch)
      (set! buf? true))))

(defn push-back-reader [^String s]
  "Creates a StringPushbackReader from a given string"
  (StringPushbackReader. s nil (.length s) false))

(defprotocol LineNumberingReader
  (get-line-number [reader]))

(defn read-line [rdr]
  (loop [c (read-char rdr) s ""]
    (if (or (= \newline c)
            (nil? c))
      s
      (recur (read-char rdr) (str s c)))))

(deftype LineNumberingPushbackReader
    [^StringPushbackReader spr ^:unsynchronized-mutable line
     ^:unsynchronized-mutable line-start? ^:unsynchronized-mutable prev]
  PushbackReader
  (read-char [reader]
    (when-let [ch (read-char spr)]
      (let [ch (if (= \return ch)
                  (let [c (peek-char spr)]
                    (when (= \formfeed c)
                      (read-char spr))
                    \newline)
                  ch)]
        (set! prev line-start?)
        (set! line-start? (= ch \newline))
        (when line-start?
          (update! line inc))
        ch)))

  (peek-char [reader]
    (peek-char spr))

  (unread [reader ch]
    (when line-start? (update! line dec))
    (set! line-start? prev)
    (unread spr ch))
  
  LineNumberingReader
  (get-line-number [reader] (inc line)))

(defn line-numbering-push-back-reader [s]
  (LineNumberingPushbackReader. (push-back-reader s) 0 true nil))

(def pbr push-back-reader)
(def lnpbr line-numbering-push-back-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- whitespace?
  "Checks whether a given character is whitespace"
  [^Character ch]
  (or (Character/isWhitespace ch) (= \, ch)))

(defn- numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (Character/isDigit ch))
   
(defn- comment-prefix?
  "Checks whether the character begins a comment."
  [ch]
  (= \; ^char ch))

(defn- number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader ^Character initch]
  (or (numeric? initch)
      (and (or (= \+ initch) (= \- initch))
           (numeric? (peek-char reader)))))

(declare read macros dispatch-macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn reader-error
  [rdr & msg]
  (throw (RuntimeException. ^String (apply str msg)))
  #_ (throw (ReaderException. (get-line rdr) (apply str msg))))

(defn macro-terminating? [ch]
  (and (not (= \# ch))
       (not (= \' ch))
       ;(not= ch \:) ;; why?
       (macros ch)))

(defn ^String read-token
  [rdr initch]
  (loop [sb (doto (StringBuilder.) (.append initch))
         ch (peek-char rdr)]
    (if (or (nil? ch)
            (whitespace? ch)
            (macro-terminating? ch))
      (.toString sb)
      (recur (doto sb (.append (read-char rdr))) (peek-char rdr)))))

(defn skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader _]
  (read-line reader)
  reader)

(def ^Pattern int-pattern #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")
(def ^Pattern ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def ^Pattern float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(deftype Pair [a b])

(defn- match-int
  [s ^Matcher m]
  (if (.group m 2)
    (if (.group m 8) 0N 0)
    (let [negate? (= "-" (.group m 1))
          a (cond
              (.group m 3) (Pair. (.group m 3) 10)
              (.group m 4) (Pair. (.group m 4) 16)
              (.group m 5) (Pair. (.group m 5) 8)
              (.group m 7) (Pair. (.group m 7) (Integer/parseInt (.group m 6)))
              :default     (Pair. nil nil))
          ^String n (.a a)
          ^int radix (.b a)]
      (when n
        (let [bn (BigInteger. n radix)
              bn (if negate? (.negate bn) bn)]
          (if (.group m 8)
            (BigInt/fromBigInteger bn)
            (if (< (.bitLength bn) 64)
              (.longValue bn)
              (BigInt/fromBigInteger bn))))))))

(defn- match-ratio
  [s ^Matcher m]
  (let [^String numinator (.group m 1)
        ^String denominator (.group m 2)]
    (/ (-> numinator   BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt)
       (-> denominator BigInteger. BigInt/fromBigInteger Numbers/reduceBigInt))))

(defn- match-float
  [^String s ^Matcher m]
  (if (.group m 4)
    (BigDecimal. ^String (.group m 1))
    (Double/parseDouble s)))

(defn- match-number
  [^String s]
  (let [m (.matcher int-pattern s)]
    (if (.matches m) (match-int s m)
        (let [m (.matcher float-pattern s)]
          (if (.matches m) (match-float s m)
              (let [m (.matcher ratio-pattern s)]
                (if (.matches m) (match-float s m))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-unicode-char
  ([^String token offset length base]
     (let [l (+ offset length)]
       (if (not (= (.length token) l))
         (throw (IllegalArgumentException. (str "Invalid unicode character: \\" token))))
      (loop [uc 0 i offset]
        (if (= i l)
          (char uc)
          (let [d (Character/digit (.charAt token i) ^int base)]
            (if (= d -1)
              (throw (IllegalArgumentException. (str "Invalid digit: " (.charAt token i))))
              (recur (long (*' uc (+' base d))) (inc' i))))))))

  ([rdr initch base length exact?]
     (let [uc (Character/digit ^char initch ^int base)]
       (if (= uc -1)
         (throw (IllegalArgumentException. (str "Invalid digit: " initch))))
       (loop [i 1 uc uc]
         (if (not (= i length))
           (let [ch (peek-char rdr)]
             (if (or (nil? ch)
                     (whitespace? ch)
                     (macros ch))
               (if exact?
                 (throw (IllegalArgumentException.
                         (str "Invalid character lenght: " i ", should be: " length)))
                 (char uc))
               (let [d (Character/digit ^char ch ^int base)
                     _ (read-char rdr)] ;; avoid unread
                 (if (= d -1)
                   (throw (IllegalArgumentException. (str "Invalid digit: " (char ch))))
                   (recur (long (inc' i)) (long (*' uc (+' base d))))))))
           (char uc))))))

(defn read-char*
  [rdr backslash]
  (let [ch (read-char rdr)]
    (if (not (nil? ch))
      (let [token (read-token rdr ch)]
        (cond

          (= 1 (.length token))  (Character/valueOf (.charAt token 0))

          (= token "newline") \newline
          (= token "space") \space
          (= token "tab") \tab
          (= token "backspace") \backspace
          (= token "formfeed") \formfeed
          (= token "return") \return

          (.startsWith token "u")
          (let [c (read-unicode-char token 1 4 16)
                ic (int c)]
            (if (and (> ic (int \uD799))
                     (< ic (int \uE000)))
              (reader-error rdr "Invalid character constant: \\u" (Integer/toString ic 16))
              c))

          (.startsWith token "o")
          (let [len (dec (.length token))]
            (if (> len 3)
              (reader-error rdr "Invalid octal escape sequence length: " len)
              (let [uc (read-unicode-char token 1 len 8)]
                (if (> (int uc) 0377)
                  (reader-error rdr "Octal escape sequence must be in range [0, 377]")
                  uc))))

          :else (reader-error rdr "Unsupported character: \\" token)))
      (reader-error rdr "EOF while reading character"))))

(defn read-past
  "Read until first character that doesn't match pred, returning
   char."
  [pred rdr]
  (loop [ch (read-char rdr)]
    (if (pred ch)
      (recur (read-char rdr))
      ch)))

(defn ^PersistentVector read-delimited-list
  [delim rdr recursive?]
  (let [first-line (if (satisfies? LineNumberingReader rdr)
                     (get-line-number rdr))]
   (loop [a (transient [])]
     (let [ch (read-past whitespace? rdr)]
       (when-not ch
         (reader-error rdr "EOF while reading"
                       (if first-line
                         (str ", starting at line" first-line))))
       (if (= delim ch)
         (persistent! a)
         (if-let [macrofn (macros ch)]
           (let [mret (macrofn rdr ch)]
             (recur (if-not (= mret rdr) (conj! a mret) a)))
           (do
             (unread rdr ch)
             (let [o (read rdr true nil recursive?)]
               (recur (if-not (= o rdr) (conj! a o) a))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare read-tagged)

(defn read-dispatch
  [rdr _]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch)
      (if-let [obj (read-tagged rdr ch)] ;; ctor reader is implemented as a taggged literal
        obj
        (reader-error rdr "No dispatch macro for " ch)))
    (reader-error rdr "EOF while reading character")))

(defn read-unmatched-delimiter
  [rdr ch]
  (reader-error rdr "Unmatched delimiter " ch))

(defn read-list
  [rdr _]
  (let [line (if (satisfies? LineNumberingReader rdr)
               (get-line-number rdr))
        the-list (read-delimited-list \) rdr true)]
    (if (empty? the-list)
      '()
      (if-not line
        (clojure.lang.PersistentList/create the-list)
        (with-meta (clojure.lang.PersistentList/create the-list) {:line line})))))

(def read-comment skip-line)

(defn read-vector
  [rdr _]
  (read-delimited-list \] rdr true))

(defn read-map
  [rdr _]
  (let [l (to-array (read-delimited-list \} rdr true))]
    (when (= 1 (bit-and (count l) 1))
      (reader-error rdr "Map literal must contain an even number of forms"))
    (RT/map l)))

(defn read-number
  [reader initch]
  (loop [sb (doto (StringBuilder.) (.append initch))
         ch (peek-char reader)]
    (if (or (nil? ch) (whitespace? ch) (macros ch))
      (let [s (.toString sb)]
        (or (match-number s)
            (reader-error reader "Invalid number format [" s "]")))
      (recur (doto sb (.append (read-char reader))) (peek-char reader)))))

(defn escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (= -1 (Character/digit ^char ch 16))
             (reader-error rdr "Invalid unicode escape: \\u" ch)
             (read-unicode-char rdr ch 16 4 true)))
      (if (Character/isDigit ^char ch)
        (let [ch (read-unicode-char rdr ch 8 3 false)]
          (if (> (int ch) 0337)
            (reader-error rdr "Octal escape sequence must be in range [0, 377]")
            ch))
        (reader-error rdr "Unsupported escape character: \\" ch)))))

(defn read-string*
  [reader _]
  (loop [sb (StringBuilder.)
         ch (read-char reader)]
    (cond
     (nil? ch) (reader-error reader "EOF while reading string")
     (= \\ ch) (recur (doto sb (.append (escape-char sb reader)))
                        (read-char reader))
     (= \" ch) (.toString sb)
     :default (recur (doto sb (.append ch)) (read-char reader)))))

(defn- ^Pair parse-symbol [^String token]
  (when (not (numeric? (.charAt token 0)))
    (let [ns-idx (.indexOf token "/")
          ns (if (not (= -1 ns-idx)) (.substring token 0 ns-idx))]
      (if (nil? ns)
        (Pair. nil token)
        (when (not (= (inc ns-idx) (count token)))
          (let [sym (.substring token (inc ns-idx))]
            (when (and (not (numeric? (.charAt sym 0)))
                     (or (= sym "/")
                         (= -1 (.indexOf sym "/"))))
              (Pair. ns sym))))))))

(defn read-symbol
  [rdr initch]
  (let [token (read-token rdr initch)]
    (case token

      ;; special symbols
      "nil" nil
      "true" true
      "false" false
      "/" '/
      
      (or (when-let [p (parse-symbol token)]
            (symbol (.a p) (.b p)))
          (reader-error rdr "Invalid token:" token)))))

(defn read-keyword
  [reader initch]
  (let [token (read-token reader (read-char reader))
        s (parse-symbol token)]
    (if (and s
             (= -1 (.indexOf token (int \.))))
      (let [^String ns (.a s)
            ^String name (.b s)]
        (if (= \: (.charAt token 0))
          (if ns
            (let [ns (symbol (.substring ns 1))
                  ns (or (find-ns ns)
                         (.lookupAlias *ns* (symbol ns)))]
              (if ns
                (keyword (str ns) name)
                (reader-error reader "Invalid token: :" token)))
            (keyword (str *ns*) (.substring name 1)))
          (keyword ns name)))
      (reader-error reader "Invalid token: :" token))))

(defn desugar-meta
  [f]
  (cond
   (symbol? f) {:tag f}
   (string? f) {:tag f}
   (keyword? f) {f true}
   :else f))

(defn wrapping-reader
  [sym]
  (fn [rdr _]
    (list sym (read rdr true nil true))))

(defn throwing-reader
  [msg]
  (fn [rdr _]
    (reader-error rdr msg)))

(defn read-meta
  [rdr _]
  (let [line (if (satisfies? LineNumberingReader rdr)
               (get-line-number rdr))
        m (desugar-meta (read rdr true nil true))]
    (when-not (map? m)
      (reader-error rdr "Metadata must be Symbol,Keyword,String or Map"))
    (let [o (read rdr true nil true)]
      (if (instance? IMeta o)
        (let [m (if (and (not (nil? line)) (instance? ISeq o)) (assoc m :line line))]
          (if (instance? IReference o)
            (reset-meta! o m)
            (with-meta o (merge (meta o) m))))
        (reader-error rdr "Metadata can only be applied to IMetas")))))

(defn read-set
  [rdr _]
  (PersistentHashSet/createWithCheck (read-delimited-list \} rdr true)))

(defn read-regex
  [rdr ch]
  (let [sb (StringBuilder.)]
    (loop [ch (read-char rdr)]
      (if (= \" ch)
        (Pattern/compile (.toString sb))
        (if (nil? ch)
          (reader-error rdr "EOF while reading regex")
          (do (.append sb ch)
              (if (= \\ ch)
                (let [ch (read-char rdr)]
                  (if (nil? ch)
                    (reader-error rdr "EOF while reading regex"))
                  (.append sb ch)
                  (recur (read-char rdr)))
                (recur (read-char rdr)))))))))

(defn read-discard
  [rdr _]
  (read rdr true nil true)
  rdr)

(defn read-var
  [rdr _]
  (list 'var (read rdr true nil true)))

(def ^:dynamic arg-env nil)

(defn read-fn
  [rdr _]
  (if arg-env
    (throw (IllegalStateException. "Nested #()s are not allowed")))
  (with-bindings {#'arg-env (sorted-map)}
    (unread rdr \()
    (let [form (read rdr true nil true) ;; this sets bindings
          rargs (rseq arg-env)
          args (if rargs
                 (let [higharg (key (first rargs))]
                   (if (pos? higharg)
                     (let [args (loop [i 0 args []]
                                  (if (> i higharg)
                                    args
                                    (recur (inc i) (conj args (get rargs i)))))
                           args (if (arg-env -1)
                                  (conj args '& (arg-env -1))
                                  args)]
                       args)))
                 [])]
      (list 'fn* args form))))

(defn- garg [n]
  (symbol (str (if (= -1 n) "rest" (str "p" n))
               "__" (RT/nextID) "#")))

(defn register-arg [n]
  (if arg-env
    (if-let [ret (arg-env n)]
      ret
      (set! arg-env (assoc arg-env n (garg n)))) ;; set! returns the value
    (throw (IllegalStateException. "Arg literal not in #()")))) ;; should never hit this

(declare read-symbol)

(defn read-arg
  [rdr pct]
  (if-not arg-env
    (read-symbol rdr pct)
    (let [ch (peek-char rdr)]
      (if (or (not ch)
              (whitespace? ch)
              (macro-terminating? ch)) ;; we hit %
        (register-arg 1)
        (let [n (read rdr true nil true)]
          (if (= n '&)
            (register-arg -1)
            (if (not (instance? Number n))
              (throw (IllegalStateException. "Arg literal must be %, %& or %integer"))
              (register-arg n))))))))

(defn read-eval
  [rdr _]
  (when-not *read-eval*
    (reader-error rdr "#= not allowed when *read-eval* is false"))
  (let [o (read rdr true nil true)]
    (if (instance? o Symbol)
      (RT/classForName (.toString ^Symbol o))
      (if (instance? IPersistentList o)
        (let [fs (first o)
              fs-name (name fs)]
          (cond
            (= fs 'var) (let [vs (second o)]
                          (RT/var (namespace vs) (name vs)))
            (.endsWith fs-name ".")
            (let [args (to-array (rest o))]
              (-> fs-name (subs 0 (dec (.length fs-name)))
                  RT/classForName (Reflector/invokeConstructor args)))

            (Compiler/namesStaticMember fs)
            (let [args (to-array (rest o))]
              (Reflector/invokeStaticMethod (namespace fs) fs-name args))

            :else
            (let [v (Compiler/maybeResolveIn *ns* fs)]
              (if (instance? Var v)
                (apply v (rest o))
                (reader-error rdr "Can't resolve " fs)))))
        (throw (IllegalArgumentException. "Unsupported #= form"))))))

(def ^:dynamic gensym-env nil)

(defn read-unquote
  [rdr comma]
  (if-let [ch (peek-char rdr)]
    (if (= \@ ch)
      ((wrapping-reader 'unquote-splicing) (doto rdr read-char) \@)
      ((wrapping-reader 'unquote) rdr \~))))

(declare syntax-quote)
(defn unquote-splicing? [form]
  (and (instance? ISeq form)
       (= (first form) 'unquote-splicing)))

(defn unquote? [form]
  (and (instance? ISeq form)
       (= (first form) 'unquote)))

(defn- expand-list [seq]
  (loop [s seq r (transient [])]
    (if s
      (let [item (first s)
            ret (conj! r
                       (cond
                         (unquote? item)          (list 'list (second item))
                         (unquote-splicing? item) (second item)
                         :else                    (list (syntax-quote item))))]
        (recur (next s) ret))
      (seq (persistent! r)))))

(defn- flatten-map [form]
  (loop [s (seq form) key-vals (transient [])]
    (if s
      (let [e (first s)]
       (recur (next s) (conj key-vals (key e) (val e))))
      (persistent! key-vals))))

(defn- register-gensym [sym]
  (if-not gensym-env
    (throw (IllegalStateException. "Gensym literal not in syntax-quote")))
  (or (get gensym-env sym)
      (set! gensym-env (assoc gensym-env
                         (symbol (str (subs (name sym)
                                            0 (dec (count (name sym))))
                                      "__" (RT/nextID) "__auto__"))))))

;; HAR HAR
(defn- resolve-symbol [s]
  (.invoke
   (doto
      (.getDeclaredMethod Compiler "resolveSymbol" (into-array Class [clojure.lang.Symbol]))
     (.setAccessible true))
   Compiler (to-array [s])))

(defn syntax-quote [form]
  (cond
    (.containsKey Compiler/specials form) (list 'quote form)

    (instance? Symbol form)
    (list 'quote
          (if (namespace form)
            (let [class? (.getMapping *ns* (symbol (namespace form)))]
              (if (instance? Class class)
                (symbol (.getName ^Class class?) (name form))
                (resolve-symbol form)))
            (let [sym (name form)]
              (cond
                (.endsWith sym "#")
                (register-gensym form)

                (.startsWith sym ".")
                form
                
                (.endsWith sym ".")
                (let [csym (symbol (subs sym (dec (count sym))))]
                  (symbol (.concat (name (resolve-symbol csym)) ".")))
                :else (resolve-symbol form)))))

    (unquote? form) (second form)
    (unquote-splicing? form) (throw (IllegalStateException. "splice not in list"))

    (instance? IPersistentCollection)
    (cond
      (instance? IRecord form) form
      (map? form) (list 'apply 'hash-map (list 'seq (cons 'concat (expand-list (seq (flatten-map form))))))
      (vector? form) (list 'apply 'vector (list 'seq (cons 'concat (expand-list (seq form)))))
      (set? form) (list 'apply 'hash-set (list 'seq (cons 'concat (expand-list (seq form)))))
      (or (instance? ISeq form) (list? form))
      (let [seq (seq form)]
        (if seq
          (list 'seq (cons 'concat (expand-list seq)))
          (cons 'list nil)))
      :else (throw (UnsupportedOperationException. "Unknown Collection type")))

    (or (keyword? form)
        (number? form)
        (char? form)
        (string? form))
    form

    :else (list 'quote form)))

(defn read-syntax-quote
  [rdr backquote]
  (with-bindings {#'gensym-env {}}
    (let [form (read rdr true nil true)
          ret (syntax-quote form)]
      (if (and (instance? IObj form)
               (dissoc (meta form) :line))
        (list 'with-meta ret (syntax-quote (meta form)))
        ret))))

(defn macros [c]
  (case c
    \" read-string*
    \: read-keyword
    \; read-comment
    \' (wrapping-reader 'quote)
    \@ (wrapping-reader 'deref)
    \^ read-meta
    \` read-syntax-quote ;;(wrapping-reader 'syntax-quote)
    \~ read-unquote
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \% read-arg
    \# read-dispatch
    nil))

(defn dispatch-macros [s]
  (case s
    \^ read-meta                       ;deprecated
    \' read-var
    \( read-fn
    \= read-eval
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \" read-regex
    \! read-comment
    \_ read-discard
    nil))

(defn read
  "Reads the first object from a PushbackReader. Returns the object read.
   If EOF, throws if eof-is-error is true. Otherwise returns sentinel."
  [reader eof-is-error sentinel is-recursive]
  (let [ch (read-char reader)]
    (cond
     (nil? ch) (if eof-is-error (reader-error reader "EOF") sentinel)
     (whitespace? ch) (recur reader eof-is-error sentinel is-recursive)
     (comment-prefix? ch) (recur (read-comment reader ch) eof-is-error sentinel is-recursive)
     :else (let [f (macros ch)
                 res
                 (cond
                  f (f reader ch)
                  (number-literal? reader ch) (read-number reader ch)
                  :else (read-symbol reader ch))]
     (if (= res reader)
       (recur reader eof-is-error sentinel is-recursive)
       res)))))

(defn read-tagged* [rdr tag]
  (let [o (read rdr true nil true)]
    (if-let [f (or (get *data-readers* tag)
                   (get default-data-readers tag))]
      (f o)
      (reader-error rdr "No reader function for tag" (name tag)))))

(defn read-ctor [rdr class-name]
  (let [class (RT/classForName (name class-name))
        ch (read-past whitespace? rdr)] ;; differs from clojure
    (if-let [[end-ch form] (case ch
                             \{ [\} :short]
                             \[ [\] :extended]
                             nil)]
      (let [entries (to-array (read-delimited-list rdr end-ch))
            all-ctors (.getConstructors class)
            ctors-num (count all-ctors)]
        (case form
          :short
          (loop [i 0]
            (if (> i ctors-num)
              (reader-error rdr "Unexpected number of constructor arguments to " (.toString class)
                            ": got" (count entries))
              (if (= (count (.getParameterTypes ^Constructor (aget all-ctors i)))
                     ctors-num)
                (Reflector/invokeConstructor class entries)
                (recur (inc i)))))
          :extended
          (let [vals (RT/map entries)]
            (loop [s (keys vals)]
              (if s
                (if-not (instance? Keyword (first s))
                  (reader-error rdr "Unreadable ctor form: key myst be of type clojure.lang.Keyword")
                  (recur (next s)))))
            (Reflector/invokeStaticMethod class "create" (to-array vals)))))
      (reader-error rdr "Invalid reader constructor form"))))

(defn read-tagged [rdr initch]
  (let [tag (read rdr true nil false)]
    (if-not (instance? Symbol tag)
      (reader-error rdr "Reader tag must be a symbol"))
    (if (.contains (name tag) ".")
      (read-ctor rdr tag)
      (read-tagged* rdr tag))))

(defn read-string
  "Reads one object from the string s"
  [s]
  (read (push-back-reader s) true nil false))
