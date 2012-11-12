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

import java.util.Comparator;
import java.util.Map;
import java.util.concurrent.Callable;

public final class I {

    public static interface Seq extends PersistentCollection {
        Object first();
        //ISeq rest ();
        Seq next();
    }

    public static interface Seqable {
        Seq seq();
    }

    public static interface Counted {
        int count();
    }

    public static interface Equiv {
        boolean equiv(Object o);
    }

    public static interface PersistentCollection extends Seqable, Equiv {
        PersistentCollection conj(Object o);
    }

    public static interface EmptyableCollection extends PersistentCollection {
        EmptyableCollection empty();
    }

    public static interface Meta {
        PersistentMap meta();
    }

    //IObj
    public static interface WithMeta extends Meta {
        WithMeta withMeta(PersistentMap meta);
    }

    //IReference
    public static interface ResetMeta extends Meta {
        ResetMeta alterMeta(Fn f, PersistentMap meta); //to avoid un-syncronizing
        ResetMeta resetMeta(PersistentMap meta);
    }

    public static interface HashEq {
        int hashEq();
    }

    public static interface Indexed extends Counted {
        Object nth(int i);
        Object nth(int i, Object notFound);
    }

    public static interface Lookup {
        Object lookup(Object key);
        Object lookup(Object key, Object notFound);
    }

    public interface MapEntry extends Map.Entry {
        Object key();
        Object val();
    }

    public interface Contains extends Lookup {
        boolean contains(Object key);
    }

    public interface Associative extends PersistentCollection, Contains {
        //MapEntry lookupEntry(Object key);
        Associative assoc(Object key, Object val);
    }

    public static interface PersistentMap extends Iterable, Associative, Counted {
        PersistentMap assoc(Object key, Object val);
        //PersistentMap assocEx(Object key, Object val);
        PersistentMap dissoc(Object key);
    }

    public static interface PersistentSet extends PersistentCollection, Counted, Contains {
        PersistentSet disj(Object key);
    }

    public static interface PersistentStack extends PersistentCollection {
        Object peek();
        PersistentStack pop();
    }

    public static interface Reversible extends Seqable {
        Seq rseq();
    }

    public static interface Sequential {}
    public static interface PersistentList extends Sequential, PersistentStack {}
    public static interface Type {}
    public static interface Record {}

    public static interface PersistentVector extends Associative, Sequential, PersistentStack, Reversible, Indexed {
        //PersistentVector assocN(int i, Object val);
    }

    public static interface Sorted {
        Comparator comparator();
        Seq sortedSeq(boolean ascending);
        Seq sortedSeqFrom(Object key, boolean ascending);
        Object entryKey(Object entry);
    }

    public static interface TransientCollection {
        TransientCollection transient_conj(Object val);
        PersistentCollection persistent();
    }

    public static interface IEditableCollection {
        TransientCollection asTransient();
    }

    public static interface TransientAssociative extends TransientCollection, ILookup {
        TransientAssociative transient_assoc(Object key, Object val);
    }

    public static interface TransientMap extends TransientAssociative, Counted /*, Iterable */{
        TransientMap transient_assoc(Object key, Object val);
        TransientMap transient_dissoc(Object key);
        PersistentMap persistent();
    }

    public static interface TransientSet extends TransientCollection, Counted, Contains {
        PersistentSet disj(Object key);
    }

    public static interface TransientVector extends TransientAssociative, Indexed, Sequential {
        //TransientVector transient_assocN(int i, Object val);
        TransientVector transient_pop();
    }

    public static interface Chunk extends Indexed {
        Chunk chunk_rest(); //dropFirst
    }

    public static interface ChunkedSeq extends Seq, Sequential {
        Chunk chunked_first();
        //Seq chuncked_rest();
        Seq chuncked_next();
    }

    public static interface Reduce {
        Object reduce(Fn f) ;
        Object reduce(Fn f, Object start) ;
    }

    public static interface KVReduce {
        Object kvreduce(Fn f, Object init);
    }

    //IOrdinal
    public static interface IndexedSeq extends Seq, Sequential, Counted {
        public int index();
    }

    public static interface Named {
        String name();
        String namespace();
    }

    public static interface Deref {
        Object deref();
    }

    public static interface Pending {
        boolean isRealized();
    }

    public static interface BlockingDeref extends Deref, Pending {
        Object derefWithTimeout(long ms, Object timeoutValue);
    }

    public static interface Validable {
        void setValidator(Fn fn);
        Fn getValidator();
        void validate(Object val);
        void validate(Fn fn, Object val);
    }

    public static interface Watchable {
        PersistentMap getWatches();
        Watchable addWatch(Object key, Fn callback);
        Watchable removeWatch(Object key);
        void notifyWatches(Object oldval, Object newval);
    }

    public static interface Ref extends Deref, Validable, Watchable {}

    public static interface Fn extends Callable, Runnable {

        Object invoke();
        Object invoke(Object a);
        Object invoke(Object a, Object b);
        Object invoke(Object a, Object b, Object c);
        Object invoke(Object a, Object b, Object c, Object d);
        Object invoke(Object a, Object b, Object c, Object d, Object e);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p, Object q);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p, Object q, Object r);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p, Object q, Object r, Object s);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p, Object q, Object r, Object s, Object t);
        Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j, Object k, Object l, Object m, Object n, Object o, Object p, Object q, Object r, Object s, Object t, Object... args);

        Object applyTo(Seq arglist) ;
    }
}
