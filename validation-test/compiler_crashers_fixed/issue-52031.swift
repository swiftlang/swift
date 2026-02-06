// RUN: %target-typecheck-verify-swift -requirement-machine-max-rule-length=4

// https://github.com/apple/swift/issues/52031

struct S<N> {}

protocol P {
  associatedtype A: P = Self
  static func f(_ x: A) -> A
}

extension S: P where N: P {
  static func f<X: P>(_ x: X) -> S<X.A> where A == X, X.A == N {
  // expected-error@-1 {{same-type constraint 'N' == 'S<S<N>>' is recursive}}
    return S<X.A>()
  }
}
