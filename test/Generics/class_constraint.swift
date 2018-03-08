// RUN: %target-typecheck-verify-swift

struct X<T: AnyObject> { } // expected-note 4{{requirement specified as 'T' : 'AnyObject'}}

class C { }
struct S { }

protocol P { }

let okay0: X<C>

struct Y<T: AnyObject> {
  let okay1: X<T>
}

struct Y2<T: C> {
  let okay2: X<T>
}

let bad0: X<C & P> // expected-error{{'X' requires that 'C & P' be a class type}}
let bad1: X<P> // expected-error{{'X' requires that 'P' be a class type}}
let bad2: X<S> // expected-error{{'X' requires that 'S' be a class type}}

struct Z<U> {
  let bad3: X<U> // expected-error{{'X' requires that 'U' be a class type}}
}

