// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

protocol P {
  associatedtype A
  static func fn(args: A)
}

class R<T> : P where T : P, T.A == T.Type {
  // expected-note@-1 {{'T' declared as parameter to type 'R'}}
  typealias A = T.Type
  static func fn(args: T.Type) {}
}

R.fn(args: R.self)
// expected-error@-1 {{generic parameter 'T' could not be inferred}}
// expected-note@-2 {{explicitly specify the generic arguments to fix this issue}}
