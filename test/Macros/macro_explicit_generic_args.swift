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

