// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5

protocol P {
  associatedtype A
}

@freestanding(expression)
macro resolve<T, U: P>(_ first: U.A, _ second: U) -> T = #externalMacro(module: "A", type: "B")
// expected-warning@-1{{external macro implementation type 'A.B' could not be found}}
// expected-note@-2{{'resolve' declared here}}

protocol Q { }

struct X: P {
  typealias A = Int
}

func test(i: Int) {
  _ = #resolve<any Q, X>(i, X())
  // expected-error@-1{{external macro implementation type 'A.B' could not be found for macro 'resolve'; plugin for module 'A' not found}}
}

@freestanding(expression)
macro OverloadedMacro<T, U>(_ x: T, _ y: U) // expected-error {{requires a definition}} expected-note {{declared here}}

@freestanding(expression)
macro OverloadedMacro<T, U>(_ x: T, _ y: U, z: Int = 0) // expected-error {{requires a definition}}

// Make sure we don't crash.
func testOverloadedMacro() {
  struct S<T> {} // expected-note 2{{'T' declared as parameter to type 'S'}}
  _ = #OverloadedMacro<S, S>
  // expected-error@-1 2{{generic parameter 'T' could not be inferred}}
  // expected-error@-2 {{missing arguments for parameters #1, #2 in macro expansion}}
}
