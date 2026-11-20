// RUN: %target-typecheck-verify-swift

protocol P0 {
  static func << (lhs: Self, rhs: Self) -> Self
}

// Satisfy operator requirement with a global function.
struct S0a : P0 { } 
func <<(lhs: S0a, rhs: S0a) -> S0a { return lhs }

// Satisfy operator requirement with a static method.
struct S0b : P0 { 
  static func <<(lhs: S0b, rhs: S0b) -> S0b { return lhs }
}

// Satisfy operator requirement with a static method in a generic struct.
struct S0c<T> : P0 {
  static func <<(lhs: S0c<T>, rhs: S0c<T>) -> S0c<T> { return lhs }
}

// A requirement shadows a protocol extension operator with the same
// signature, so the unavailable operator doesn't satisfy the requirement.
infix operator <~>

protocol P1 {
  static func <~> (lhs: Self, rhs: Self) -> Bool // expected-note {{protocol requires function '<~>' with type '(S1, S1) -> Bool'}}
}

extension P1 {
  @available(*, unavailable)
  static func <~> (lhs: Self, rhs: Self) -> Bool { fatalError() }
}

struct S1 : P1 { }
// expected-error@-1 {{type 'S1' does not conform to protocol 'P1'}}
// expected-note@-2 {{add stubs for conformance}}
