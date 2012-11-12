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

import java.util.Map;

public final class A {

    public abstract static class Reference implements I.ResetMeta {
        private I.PersistentMap _meta;

        public Reference() {
            this(null);
        }

        public Reference(I.PersistentMap meta) {
            _meta = meta;
        }

        synchronized public I.PersistentMap meta() {
            return _meta;
        }

        synchronized public I.PersistentMap alterMeta(I.Fn alter, I.Seq args)  {
            _meta = (I.PersistentMap) alter.applyTo(new Cons(_meta, args));
            return _meta;
        }

        synchronized public I.PersistentMap resetMeta(I.PersistentMap m) {
            _meta = m;
            return m;
        }
    }

    public abstract static class Ref extends Reference implements I.Ref{
        protected volatile I.Fn validator = null;
        private volatile I.PersistentMap watches = RT.map(null);

        public Ref() {
            super();
        }

        public Ref(I.PersistentMap meta) {
            super(meta);
        }

        void validate(I.Fn vf, Object val) {
            try {
                if(vf != null && !RT.BOOLEAN(vf.invoke(val)))
                    throw new IllegalStateException("Invalid reference state");
            } catch(RuntimeException re) {
                throw re;
            } catch(Exception e) {
                throw new IllegalStateException("Invalid reference state", e);
            }
        }

        void validate(Object val) {
            validate(validator, val);
        }

        public void setValidator(I.Fn vf) {
            try {
                validate(vf, deref());
            } catch(Exception e) {
                throw Util.sneakyThrow(e);
            }
            validator = vf;
        }

        public I.Fn getValidator(){
            return validator;
        }

        public I.PersistentMap getWatches() {
            return watches;
        }

        synchronized public I.Ref addWatch(Object key, I.Fn callback) {
            watches = RT.assoc(watches, key, callback);
            return this;
        }

        synchronized public I.Ref removeWatch(Object key) {
            try {
                watches = RT.dissoc(watches, key);
            } catch(Exception e) {
                throw Util.sneakyThrow(e);
            }
            return this;
        }

        public void notifyWatches(Object oldval, Object newval) {
            I.PersistentMap ws = watches;
            if(ws.count() > 0) {
                for(I.Seq s = RT.seq(ws); s != null; s = RT.next(s)) {
                    Map.Entry e = (Map.Entry) RT.first(s);
                    I.Fn fn = (I.Fn) RT.value(e);
                    try {
                        if(fn != null)
                            fn.invoke(RT.key(e), this, oldval, newval);
                    } catch(Exception e1) {
                        throw Util.sneakyThrow(e1);
                    }
                }
            }
        }
    }

    public abstract static class Fn implements I.Fn {

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
            return throwArity(0);
        }

        public Object invoke(Object a) {
            return throwArity(1);
        }

        public Object invoke(Object a, Object b) {
            return throwArity(2);
        }

        public Object invoke(Object a, Object b, Object c) {
            return throwArity(3);
        }

        public Object invoke(Object a, Object b, Object c, Object d) {
            return throwArity(4);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e) {
            return throwArity(5);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f) {
            return throwArity(6);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            return throwArity(7);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h) {
            return throwArity(8);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i) {
            return throwArity(9);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j) {
            return throwArity(10);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k) {
            return throwArity(11);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l) {
            return throwArity(12);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m) {
            return throwArity(13);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m, Object n) {
            return throwArity(14);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o) {
            return throwArity(15);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p) {
            return throwArity(16);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p, Object q) {
            return throwArity(17);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p, Object q, Object r) {
            return throwArity(18);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p, Object q, Object r, Object s) {
            return throwArity(19);
        }

        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p, Object q, Object r, Object s, Object t) {
            return throwArity(20);
        }


        public Object invoke(Object a, Object b, Object c, Object d, Object e, Object f, Object g,
                             Object h, Object arg9, Object i, Object j, Object k, Object l, Object m,
                             Object n, Object o, Object p, Object q, Object s, Object t,
                             Object... args) {
            return throwArity(21);
        }

        public Object applyTo(I.Seq arglist) {
            return applyToHelper(this, Util.ret1(arglist,arglist = null));
        }

        static public Object applyToHelper(I.Fn ifn, I.Seq arglist) {
            switch(Util.boundedLength(arglist, 20))
                {
                case 0:
                    arglist = null;
                    return ifn.invoke();
                case 1:
                    return ifn.invoke(Util.ret1(arglist.first(),arglist = null));
                case 2:
                    return ifn.invoke(arglist.first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 3:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 4:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 5:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 6:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 7:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 8:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 9:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 10:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 11:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 12:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 13:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 14:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 15:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 16:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 17:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 18:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 19:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                case 20:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , Util.ret1((arglist = arglist.next()).first(),arglist = null)
                    );
                default:
                    return ifn.invoke(arglist.first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , (arglist = arglist.next()).first()
                                      , RT.seqToArray(Util.ret1(arglist.next(),arglist = null)));
                }
        }

        public Object throwArity(int n){
            String name = getClass().getSimpleName();
            int suffix = name.lastIndexOf("__"); // "->" == __ARROW_ broken
            throw new ArityException(n, (suffix == -1 ? name : name.substring(0, suffix)).replace('_', '-'));
        }
    }
}
