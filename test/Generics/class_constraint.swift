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

// SR-7168: layout constraints weren't getting merged.
protocol P1 {
  associatedtype A
  var a: A { get }
}

protocol P2 {
  associatedtype B: P1
  var b: B { get }
}

func requiresAnyObject<T: AnyObject>(_: T) { }

func anyObjectConstraint<T: P2, U: P2>(_ t: T, _ u: U)
where T.B.A: AnyObject, U.B: AnyObject, T.B == T.B.A, U.B.A == U.B {
  requiresAnyObject(t.b)
  requiresAnyObject(u.b)
  requiresAnyObject(t.b.a)
  requiresAnyObject(u.b.a)
}
