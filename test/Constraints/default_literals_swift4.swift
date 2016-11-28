// RUN: %target-typecheck-verify-swift -swift-version 4

// Swift 3 used default literal types even for normal protocol constraints,
// which led to nonsensical type inference behavior.

// expected-note@+1{{in call to function 'f'}}
func f<T: ExpressibleByIntegerLiteral>(_: T = 0) { }

f() // expected-error{{generic parameter 'T' could not be inferred}}

// expected-note@+1{{'T' declared as parameter to type 'X'}}
struct X<T: ExpressibleByIntegerLiteral> {
  func g() { }
}

X().g() // expected-error{{generic parameter 'T' could not be inferred}}
// expected-note@-1{{explicitly specify the generic arguments to fix this issue}}
