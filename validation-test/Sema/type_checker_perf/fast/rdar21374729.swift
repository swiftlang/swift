// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

protocol P {
  associatedtype A
  static func fn(args: A)
}

class R<T> : P where T : P, T.A == T.Type {
  typealias A = T.Type
  static func fn(args: T.Type) {}
}

R.fn(args: R.self)
// expected-error@-1 {{type of expression is ambiguous without more context}}
