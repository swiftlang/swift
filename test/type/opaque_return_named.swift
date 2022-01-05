// RUN: %target-typecheck-verify-swift -enable-experimental-named-opaque-types -disable-availability-checking

// Tests for experimental extensions to opaque return type support.

func f0() -> <T> T { return () }
func f1() -> <T, U, V> T { () }
// FIXME better diagnostic: expected-error@-1{{generic parameter 'U' could not be inferred}}
// FIXME better diagnostic: expected-error@-2{{generic parameter 'V' could not be inferred}}
func f2() -> <T: Collection, U: SignedInteger> T { // expected-note{{required by opaque return type of global function 'f2()'}}
  () // expected-error{{type '()' cannot conform to 'Collection'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
  // expected-error@-2{{generic parameter 'U' could not be inferred}}
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
