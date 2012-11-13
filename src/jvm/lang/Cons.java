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

import java.io.Serializable;

final public class Cons extends A.Seq implements Serializable {

    private final Object _first;
    private final I.Seq _next;

    public Cons(Object first, I.Seq next){
        this._first = first;
        this._next = next;
    }

    public Cons(IPersistentMap meta, Object first, I.Seq next){
        super(meta);
        this._first = first;
        this._next = next;
    }

    public Object first(){
        return _first;
    }

    public ISeq next(){
        return _next;
    }

    // public ISeq rest(){
    //     if(_next == null)
    //         return PersistentList.EMPTY;
    //     return _next;
    // }

    public int count(){
        return 1 + RT.count(_next);
    }

    public Cons withMeta(I.PersistentMap meta){
        return new Cons(meta, _first, _next);
    }
}
