(ns clojure.lang.ns
  (:refer-clojure :exclude [*ns* find-ns ns-aliases ns-unalias the-ns ns-map ns-resolve])
  (:require [clojure.lang.runtime :refer [*ns*]])
  (:import (clojure.lang RT Var)))

;;TODO Areference
(defrecord Namespace [name mappings aliases]
  Object
  (toString [this] (str name)))

;; import becomes an alias to alias
(def default-aliases
  '{Boolean                         java.lang.Boolean
    Byte                            java.lang.Byte
    Character                       java.lang.Character
    Class                           java.lang.Class
    ClassLoader                     java.lang.ClassLoader
    Compiler                        clojure.lang.Compiler
    Double                          java.lang.Double
    Enum                            java.lang.Enum
    Float                           java.lang.Float
    InheritableThreadLocal          java.lang.InheritableThreadLocal
    Integer                         java.lang.Integer
    Long                            java.lang.Long
    Math                            java.lang.Math
    Number                          java.lang.Number
    Object                          java.lang.Object
    Package                         java.lang.Package
    Process                         java.lang.Process
    ProcessBuilder                  java.lang.ProcessBuilder
    Runtime                         java.lang.Runtime
    RuntimePermission               java.lang.RuntimePermission
    SecurityManager                 java.lang.SecurityManager
    Short                           java.lang.Short
    StackTraceElement               java.lang.StackTraceElement
    StrictMath                      java.lang.StrictMath
    String                          java.lang.String
    StringBuffer                    java.lang.StringBuffer
    StringBuilder                   java.lang.StringBuilder
    System                          java.lang.System
    Thread                          java.lang.Thread
    ThreadGroup                     java.lang.ThreadGroup
    ThreadLocal                     java.lang.ThreadLocal
    Throwable                       java.lang.Throwable
    Void                            java.lang.Void
    Appendable                      java.lang.Appendable
    CharSequence                    java.lang.CharSequence
    Cloneable                       java.lang.Cloneable
    Comparable                      java.lang.Comparable
    Iterable                        java.lang.Iterable
    Readable                        java.lang.Readable
    Runnable                        java.lang.Runnable
    Callable                        java.util.concurrent.Callable
    BigInteger                      java.math.BigInteger
    BigDecimal                      java.math.BigDecimal
    ArithmeticException             java.lang.ArithmeticException
    ArrayIndexOutOfBoundsException  java.lang.ArrayIndexOutOfBoundsException
    ArrayStoreException             java.lang.ArrayStoreException
    ClassCastException              java.lang.ClassCastException
    ClassNotFoundException          java.lang.ClassNotFoundException
    CloneNotSupportedException      java.lang.CloneNotSupportedException
    EnumConstantNotPresentException java.lang.EnumConstantNotPresentException
    Exception                       java.lang.Exception
    IllegalAccessException          java.lang.IllegalAccessException
    IllegalArgumentException        java.lang.IllegalArgumentException
    IllegalMonitorStateException    IllegalMonitorStateException
    IllegalStateException           java.lang.IllegalStateException
    IllegalThreadStateException     java.lang.IllegalThreadStateException
    IndexOutOfBoundsException       java.lang.IndexOutOfBoundsException
    InstantiationException          java.lang.InstantiationException
    InterruptedException            java.lang.InterruptedException
    NegativeArraySizeException      java.lang.NegativeArraySizeException
    NoSuchFieldException            java.lang.NoSuchFieldException
    NoSuchMethodException           java.lang.NoSuchMethodException
    NullPointerException            java.lang.NullPointerException
    NumberFormatException           java.lang.NumberFormatException
    RuntimeException                java.lang.RuntimeException
    SecurityException               java.lang.SecurityException
    StringIndexOutOfBoundsException java.lang.StringIndexOutOfBoundsException
    TypeNotPresentException         java.lang.TypeNotPresentException
    UnsupportedOperationException   java.lang.UnsupportedOperationException
    AbstractMethodError             java.lang.AbstractMethodError
    AssertionError                  java.lang.AssertionError
    ClassCircularityError           java.lang.ClassCircularityError
    ClassFormatError                java.lang.ClassFormatError
    Error                           java.lang.Error
    ExceptionInInitializerError     java.lang.ExceptionInInitializerError
    IllegalAccessError              java.lang.IllegalAccessError
    IncompatibleClassChangeError    java.lang.IncompatibleClassChangeError
    InstantiationError              java.lang.InstantiationError
    InternalError                   java.lang.InternalError
    LinkageError                    java.lang.LinkageError
    NoClassDefFoundError            java.lang.NoClassDefFoundError
    NoSuchFieldError                java.lang.NoSuchFieldError
    NoSuchMethodError               java.lang.NoSuchMethodError
    OutOfMemoryError                java.lang.OutOfMemoryError
    StackOverflowError              java.lang.StackOverflowError
    ThreadDeath                     java.lang.ThreadDeath
    UnknownError                    java.lang.UnknownError
    UnsatisfiedLinkError            java.lang.UnsatisfiedLinkError
    UnsupportedClassVersionError    java.lang.UnsupportedClassVersionError
    VerifyError                     java.lang.VerifyError
    VirtualMachineError             java.lang.VirtualMachineError
    Thread$UncaughtExceptionHandler java.lang.Thread$UncaughtExceptionHandler
    Thread$State                    java.lang.Thread$State
    Deprecated                      java.lang.Deprecated
    Override                        java.lang.Override
    SuppressWarnings                java.lang.SuppressWarnings})

(defn make-ns [name]
  (->Namespace name {} default-aliases))

(defonce namespaces (atom {'clojure.core (make-ns 'clojure.core)
                           'user         (make-ns 'user)}))

(defn find-ns [ns]
  (@namespaces ns))

(defn the-ns [ns]
  (if (instance? Namespace ns)
    ns
    (or (find-ns ns)
        (throw (Exception. (str "Namespace " ns " not found"))))))

(defn set-namespace [name ns]
  (swap! namespaces assoc name ns))

(defn ns-aliases [ns]
  (:aliases (the-ns ns)))

(defn ns-map [ns]
  (:mappings (the-ns ns)))

(defn ns-unalias [ns sym]
  (let [ns (the-ns ns)
        unaliased (assoc ns :aliases
                         (dissoc (ns-aliases ns) sym))]
    (reset! namespace unaliased)))

(defn ns-alias
  ([sym] (ns-alias *ns* sym))
  ([ns sym] ((ns-aliases ns) sym)))

(defn resolve-ns
  ([sym] (resolve-ns *ns* sym))
  ([in-ns sym]
     (or (ns-alias in-ns sym)
         (:name (find-ns sym)))))

(defn find-interned-var
  ([sym] (find-interned-var *ns* sym))
  ([ns sym]
     (when-let [o ((ns-map ns) sym)]
       (when (and (instance? Var o)
                  (= ns (.name (.ns ^Var o))))
         o))))

(defn maybe-resolve [ns sym]
  (if-not (nil? (namespace sym))
    (when-let [ns (resolve-ns ns sym)]
      (find-interned-var ns (symbol (name sym))))
    (if (or (and (pos? (-> sym name (.indexOf ".")))
                 (not  (-> sym name (.endsWith "."))))
            (= \[ (-> sym name (.charAt 0))))
      (RT/classForName (name sym))
      (if (= sym 'ns)
        #'ns
        (if (= sym 'in-ns)
          #'in-ns
          ((ns-map ns) sym))))))

(defn ns-resolve
  ([ns sym] (ns-resolve ns nil sym))
  ([ns env sym]
     (when-not (contains? env sym)
       (maybe-resolve (the-ns ns) sym))))
