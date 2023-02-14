// RUN: %target-typecheck-verify-swift -enable-experimental-named-opaque-types -disable-availability-checking

// Tests for experimental extensions to opaque return type support.

func f0() -> <T> T { return () }
func f1() -> <T, U, V> T { () } // expected-error{{underlying type for opaque result type 'T' could not be inferred from return expression}}
func f2() -> <T: Collection, U: SignedInteger> T { // expected-note{{required by opaque return type of global function 'f2()'}}
  () // expected-error{{type '()' cannot conform to 'Collection'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
  // expected-error@-2{{underlying type for opaque result type 'T' could not be inferred from return expression}}
}

func f4() async -> <T> T { () }

func g0() -> <T> { } // expected-error{{expected type for function result}}
func g1() -> async <T> T { () } // expected-error{{'async' may only occur before '->'}}
func g2() -> <T> T async { () } // expected-error{{'async' may only occur before '->'}}

let x0: <T> Int = 1
var x1: <T> (Int, Int) = (1, 1)
var x2: <T> (<U> Int, Int) = (1, 1) // expected-error{{expected type}} expected-error{{cannot convert value of type '(Int, Int)' to specified type 'Int'}}
for _: <T> Int in [1, 2, 3] { } // FIXME mention of 'some' is wrong: expected-error{{some' type can only be declared on a single property declaration}}

struct S0 { subscript(i: Int) -> <T> T { 1 } }

protocol P { }
extension Int: P { }

protocol Q { }

func h0() -> <T: P> T { 5 }

func h1() -> <T: P> T { // expected-note{{opaque return type declared here}}
  3.14159 // expected-error{{return type of global function 'h1()' requires that 'Double' conform to 'P'}}
}

func h2() -> <T: P & Q> T { // expected-note{{opaque return type declared here}}
  3 // expected-error{{return type of global function 'h2()' requires that 'Int' conform to 'Q'}}
}

func test_h0() {
  // Make sure we can treat h0 as a P
  let _: P = h0()
}

protocol P1 {
  associatedtype A
}

protocol Q1 {
  associatedtype B
}

extension Int: P1 {
  typealias A = String
}

extension String: P1 {
  typealias A = String
}

extension Double: Q1 {
  typealias B = String
}

extension String: Q1 {
  typealias B = Int
}

func f1() -> <T: P1, U where U: Q1> (T, U) {
  return (3, 3.14159)
}

func f2(cond: Bool) -> <T: P1, U where U: Q1> (T, U) {
  if cond {
    return (3, 3.14159)
  } else {
    return (3, 2.71828)
  }
}

func f3(cond: Bool) -> <T: P1, U where U: Q1> (T, U) {
  // expected-error@-1{{function declares an opaque return type 'U', but the return statements in its body do not have matching underlying types}}
  if cond {
    return (3, 3.14159) // expected-note{{return statement has underlying type 'Double'}}
  } else {
    return (3, "hello") // expected-note{{return statement has underlying type 'String'}}
  }
}

func f4(cond: Bool) -> <T: P1, U where U: Q1> (T, U) {
  if cond {
    return (3, 3.14159)
  } else {
  }
}

func f5(cond: Bool) -> <T: P1, U where U: Q1> (T, U) { }
// expected-error@-1{{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}

protocol DefaultInitializable {
  init()
}

extension String: DefaultInitializable { }
extension Int: DefaultInitializable { }

struct Generic<T: P1 & Equatable & DefaultInitializable>
    where T.A: DefaultInitializable {
  var value: T

  func f() -> <U: Q1, V: P1 where U: Equatable, V: Equatable> (U, V) {
    return ("hello", value)
  }

  subscript(index: Int) -> <U: Q1, V: P1 where U: Equatable, V: Equatable> (U, V) {
    return ("hello", value)
  }

  var property: <U: Q1, V: P1 where U: Equatable, V: Equatable> (U, V) {
    return ("hello", value)
  }

  var property2: <U: Q1, V: P1 where U: Equatable, V: Equatable> (U, V) =
    ("hello", T())

  var property3: <U: Q1, V: P1 where U: Equatable, V: Equatable> (U, V) =
    ("hello", T()) {
    didSet {
      print("here")
    }
  }

  func sameTypeParams() -> <C: Collection where C.Element == T.A> C {
    [ T.A(), T.A() ]
  }

  func sameTypeParamsBad() -> <C: Collection where C.Element == T.A> C {
    [ T() ] // expected-error{{cannot convert value of type 'T' to expected element type 'T.A'}}
  }
}

func testGeneric(i: Int, gs: Generic<String>, gi: Generic<Int>) {
  let gs1 = gs.f()
  let gs2 = gs.f()
  _ = (gs1 == gs2)

  let gi1 = gi.f()
  // FIXME: Diagnostic below is correct, but a bit misleading because these are different Us and Vs.
  _ = (gs1 == gi1) // expected-error{{binary operator '==' cannot be applied to operands of type '(U, V)' and '(U, V)'}}

  _ = gs[i]
}
