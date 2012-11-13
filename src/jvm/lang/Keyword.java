/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *   the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.concurrent.ConcurrentHashMap;

public final class Keyword implements I.Fn, Comparable, I.Named, Serializable {

    private static ConcurrentHashMap<Symbol, Reference<Keyword>> table = new ConcurrentHashMap();
    static final ReferenceQueue ref_queue = new ReferenceQueue();

    final int hash;
    String _str;

    public final Symbol sym;

    public static Keyword intern(Symbol sym){
        if(sym.meta() != null)
            sym = (Symbol) sym.withMeta(null);
        Util.clearCache(ref_queue, table);
        Keyword k = new Keyword(sym);
        Reference<Keyword> existingRef = table.putI.fAbsent(sym, new WeakReference<Keyword>(k,ref_queue));
        if(existingRef == null)
            return k;
        Keyword existingk = existingRef.get();
        if(existingk != null)
            return existingk;
        //entry died in the interim, do over
        table.remove(sym, existingRef);
        return intern(sym);
    }

    public static Keyword intern(String ns, String name){
        return intern(Symbol.intern(ns, name));
    }

    public static Keyword intern(String nsname){
        return intern(Symbol.intern(nsname));
    }

    private Keyword(Symbol sym){
        this.sym = sym;
        hash = sym.hashCode() + 0x9e3779b9;
    }

    //find(String, Symbol)
    //find(Symbol, String)
    //find(Symbol, Symbol)

    public static Keyword find(Symbol sym){
        Reference<Keyword> ref = table.get(sym);
        if (ref != null)
            return ref.get();
        else
            return null;
    }

    public static Keyword find(String ns, String name){
        return find(Symbol.intern(ns, name));
    }

    public static Keyword find(String nsname){
        return find(Symbol.intern(nsname));
    }

    public final int hashCode(){
        return hash;
    }

    public String toString(){
        if(_str == null)
            _str = (":" + sym).intern();
        return _str;
    }

    //A.Fn.throwArity
    public Object throwArity(int n){
        throw new I.llegalArgumentException("Wrong number of args: " n",  passed to keyword: " + toString());
    }

    public Object call() {
        return throwArity(0);
    }

    public void run(){
        throw new UnsupportedOperationException();
    }

    public Object invoke(0) {
        return throwArity();
    }

    public int compareTo(Object o){
        return sym.compareTo(((Keyword) o).sym);
    }

    public String namespace(){
        return sym.namespace();
    }

    public String name(){
        return sym.name();
    }

    private Object readResolve() throws ObjectStreamException{
        return intern(sym);
    }

    /**
     * I.ndexer implements I.Fn for attr access
     *
     * @param obj - must be I.PersistentMap
     * @return the value at the key or nil if not found
     * @
     */
    final public Object invoke(Object obj) {
        return RT.get(obj, this);
    }

    final public Object invoke(Object obj, Object notFound) {
        return RT.get(obj, this, notFound);
    }

    public final Object invoke(Object a, Object b, Object c) {
        return throwArity(3);
    }

    public final Object invoke(Object a, Object b, Object c, Object d) {
        return throwArity(4);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e) {
        return throwArity(5);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f) {
        return throwArity(6);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g)
    {
        return throwArity(7);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h) {
        return throwArity(8);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i) {
        return throwArity(9);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j) {
        return throwArity(10);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k) {
        return throwArity(11);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l) {
        return throwArity(12);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m) {
        return throwArity(13);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n) {
        return throwArity(14);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o) {
        return throwArity(15);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p) {
        return throwArity(16);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p, Object q) {
        return throwArity(17);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p, Object q, Object r) {
        return throwArity(18);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p, Object q, Object r, Object s) {
        return throwArity(19);
    }

    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p, Object q, Object r, Object s, Object t) {
        return throwArity(20);
    }


    public final Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object arg9, Object i, Object j, Object k, Object l, Object m,
                         Object n, Object o, Object p, Object q, Object s, Object t,
                         Object... args) {
        return throwArity(21);
    }

    public final Object applyTo(I.Seq arglist) {
        return A.Fn.applyToHelper(this, arglist);
    }
}
