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

import java.util.concurrent.atomic.AtomicBoolean;

public final class Var extends A.Ref implements I.Fn, I.Ref {

    volatile Object root;
    transient final AtomicBoolean threadBound;

    //small performance gain
    // volatile boolean dynamic = false;
    // volatile boolean macro = false;
    // volatile boolean private = false;
    // volatile Symbol tag = null;

    public final Symbol sym;
    public final Namespace ns;

    static class TBox {

        volatile Object val;
        final Thread thread;

        public TBox(Thread t, Object val) {
            this.thread = t;
            this.val = val;
        }
    }

    public static class Unbound extends A.Fn {
        final public Var v;

        public Unbound(Var v) {
            this.v = v;
        }

        public String toString() {
            return "Unbound: " + v;
        }

        public Object throwArity(int n) {
            throw new IllegalStateException("Attempting to call unbound fn: " + v);
        }
    }

    static class Frame {
        //Var->TBox
        I.Associative bindings;
        //Var->val
        Frame prev;

        public Frame() {
            this(RT.map(null), null);
        }

        public Frame(I.Associative bindings, Frame prev) {
            this.bindings = bindings;
            this.prev = prev;
        }

        protected Object clone() {
            Frame f = new Frame();
            f.bindings = this.bindings;
            return f;
        }

    }

    static final ThreadLocal<Frame> dvals = new ThreadLocal<Frame>() {
        protected Frame initialValue() {
            return new Frame();
        }
    };

    // never used
    // public static Frame getThreadBindingFrame() {
    //     Frame f = dvals.get();
    //     if(f != null)
    //         return f;
    //     return new Frame();
    // }

    public static Frame cloneThreadBindingFrame() {
        Frame f = dvals.get();
        if(f != null)
            return f.clone();
        return new Frame();
    }

    public static void resetThreadBindingFrame(Frame frame) {
        dvals.set(frame);
    }

    public static Var intern(Symbol nsName, Symbol sym) {
        Namespace ns = Namespace.findOrCreate(nsName);
        return intern(ns, sym);
    }

    public static Var intern(Namespace ns, Symbol sym) {
        return ns.intern(sym);
    }

    public static Var intern(Namespace ns, Symbol sym, Object root) {
        return intern(ns, sym, root, true);
    }

    public static Var intern(Namespace ns, Symbol sym, Object root, boolean replaceRoot) {
        Var dvout = ns.intern(sym);
        if(!dvout.hasRoot() || replaceRoot)
            dvout.bindRoot(root);
        return dvout;
    }

    //ns.name -> ns.sym
    public String toString() {
        if(ns != null)
            return "#'" + ns.sym + "/" + sym;
        return "#<Var: " + (sym != null ? sym.toString() : "--unnamed--") + ">";
    }

    public static Var find(Symbol nsQualifiedSym) {
        if(nsQualifiedSym.namespace() == null)
            throw new IllegalArgumentException("Symbol must be namespace-qualified");
        Namespace ns = Namespace.find(Symbol.intern(nsQualifiedSym.namespace()));
        if(ns == null)
            throw new IllegalArgumentException("No such namespace: " + nsQualifiedSym.namespace());
        return ns.findInternedVar(Symbol.intern(nsQualifiedSym.name()));
    }

    public static Var create() {
        return new Var(null, null);
    }

    public static Var create(Object root) {
        return new Var(null, null, root);
    }

    Var(Namespace ns, Symbol sym) {
        this.ns = ns;
        this.sym = sym;
        this.threadBound = new AtomicBoolean(false);
        this.root = new Unbound(this);
        //setMeta(RT.map(null));
        setMeta(null);
    }

    Var(Namespace ns, Symbol sym, Object root) {
        this(ns, sym);
        this.root = root;
    }

    public boolean isBound() {
        return hasRoot() || (threadBound.get() && dvals.get().bindings.containsKey(this));
    }

    // public final Object get() {
    //     if(!threadBound.get())
    //         return root;
    //     return deref();
    // }

    public final Object deref() {
        TBox b = getThreadBinding();
        if(b != null)
            return b.val;
        return root;
    }

    public void setValidator(I.Fn vf) {
        if(hasRoot())
            validate(vf, root);
        validator = vf;
    }

    public Object alter(I.Fn fn, I.Seq args) {
        set(fn.applyTo(RT.cons(deref(), args)));
        return this;
    }

    public Object set(Object val) {
        validate(val);
        TBox b = getThreadBinding();
        if(b != null) {
                if(Thread.currentThread() != b.thread)
                    throw new IllegalStateException("Can't set!: " + sym + " from non-binding thread");
                return (b.val = val);
            }
        throw new IllegalStateException("Can't change/establish root binding of: " + sym +" with set");
    }


    public void setMeta(I.PersistentMap m) {
        //ensure these basis keys
        resetMeta(m.assoc(RT.NAME_KEY, sym).assoc(RT.NS_KEY, ns));
    }

    public Var setDynamic() {
        setDynamic(true);
        return this;
    }

    public Var setDynamic(boolean b) {
        try {
            alterMeta(RT.ASSOC, RT.cons(DYNAMIC_KEY, b));
        } catch (Exception e) {
            throw Util.sneakyThrow(e);
        }
        return this;
    }

    public final boolean isDynamic() {
        return RT.BOOLEAN(meta().valAt(DYNAMIC_KEY));
    }

    public Var setMacro() {
        setMacro(true);
        return this;
    }

    public Var setMacro(boolean b) {
        try {
            alterMeta(RT.ASSOC, RT.cons(MACRO_KEY, b));
        } catch (Exception e) {
            throw Util.sneakyThrow(e);
        }
        return this;
    }

    public final boolean isMacro() {
        return RT.BOOLEAN(meta().valAt(MACRO_KEY));
    }

    public final boolean isPublic() {
        return !RT.BOOLEAN(meta().valAt(PRIVATE_KEY));
    }

    public Object getTag() {
        return meta().valAt(RT.TAG_KEY);
    }

    public Var setTag(Symbol tag) {
        try {
            alterMeta(RT.ASSOC, RT.cons(TAG_KEY, b));
        } catch (Exception e) {
            throw Util.sneakyThrow(e);
        }
        return this;
    }

    public final Object getRawRoot() {
        return root;
    }

    public final boolean hasRoot() {
        return !(root instanceof Unbound);
    }

    //binding root always clears macro flag
    synchronized public void bindRoot(Object root) {
        validate(root);
        Object oldroot = this.root;
        this.root = root;
        try {
            alterMeta(RT.DISSOC, RT.list(MACRO_KEY)); //RT.list(MACRO_KEY)
        } catch (Exception e) {
            throw Util.sneakyThrow(e);
        }
        notifyWatches(oldroot,this.root);
    }

    // never used
    // synchronized void swapRoot(Object root) {
    //     validate(getValidator(), root);
    //     Object oldroot = this.root;
    //     this.root = root;
    //     notifyWatches(oldroot,root);
    // }

    synchronized public void unbindRoot() {
        this.root = new Unbound(this);
    }

    // never used
    // synchronized public void commuteRoot(I.Fn fn) {
    //     Object newRoot = fn.invoke(root);
    //     validate(getValidator(), newRoot);
    //     Object oldroot = root;
    //     this.root = newRoot;
    //     ++rev;
    //     notifyWatches(oldroot,newRoot);
    // }

    synchronized public Object alterRoot(I.Fn fn, I.Seq args) {
        Object newRoot = fn.applyTo(RT.cons(root, args));
        validate(newRoot);
        Object oldroot = root;
        this.root = newRoot;
        notifyWatches(oldroot,newRoot);
        return newRoot;
    }

    public static void pushThreadBindings(I.Associative bindings) {
        Frame f = dvals.get();
        I.Associative bmap = f.bindings;
        for(I.Seq bs = bindings.seq(); bs != null; bs = bs.next()) {
            I.MapEntry e = (I.MapEntry) bs.first();
            Var v = (Var) e.key();
            if(!v.isDynamic())
                throw new IllegalStateException("Can't dynamically bind non-dynamic var: " v.ns "/" v.sym));
                v.validate(v.getValidator(), e.val());
                v.threadBound.set(true);
                bmap = bmap.assoc(v, new TBox(Thread.currentThread(), e.val()));
            }
        dvals.set(new Frame(bmap, f));
    }

    public static void popThreadBindings() {
        Frame f = dvals.get();
        if(f.prev == null)
            throw new IllegalStateException("Pop without matching push");
        dvals.set(f.prev);
    }

    public static Associative getThreadBindings() {
        Frame f = dvals.get();
        I.PersistentMap ret = PersistentHashMap.EMPTY;
        for(I.Seq bs = f.bindings.seq(); bs != null; bs = bs.next())
            {
                I.MapEntry e = (I.MapEntry) bs.first();
                Var v = (Var) e.key();
                TBox b = (TBox) e.val();
                ret = ret.assoc(v, b.val);
            }
        return ret;
    }

    public final TBox getThreadBinding() {
        if(threadBound.get()) {
            I.MapEntry e = dvals.get().bindings.entryAt(this);
            if(e != null)
                return (TBox) e.val();
        }
        return null;
    }

    final public I.Fn fn() {
        return (I.Fn) deref();
    }

    public Object call() {
        return invoke();
    }

    public void run() {
        try {
            invoke();
        } catch(Exception e) {
            throw Util.sneakyThrow(e);
        }
    }

    public Object invoke() {
        return fn().invoke();
    }

    public Object invoke(Object a) {
        return fn().invoke(a);
    }

    public Object invoke(Object a, Object b) {
        return fn().invoke(a, b);
    }

    public Object invoke(Object a, Object b, Object c) {
        return fn().invoke(a, b, c);
    }

    public Object invoke(Object a, Object b, Object c, Object d) {
        return fn().invoke(a, b, c, d);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e) {
        return fn().invoke(a, b, c, d, e);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f) {
        return fn().invoke(a, b, c, d, e, f);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        return fn().invoke(a, b, c, d, e, f, g);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h) {
        return fn().invoke(a, b, c, d, e, f, g, h);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i) {
        return fn().invoke(a, b, c, d, e, f, g, h, i);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p, Object q) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p, Object q, Object r) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p, Object q, Object r, Object s) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p, Object q, Object r, Object s, Object t) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t);
    }

    public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                         Object h, Object i, Object j, Object k, Object l, Object m, Object n,
                         Object o, Object p, Object q, Object r, Object s, Object t, Object... args) {
        return fn().invoke(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, args);
    }

    public Object applyTo(I.Seq arglist) {
        return A.Fn.applyToHelper(this, arglist);
    }
}
