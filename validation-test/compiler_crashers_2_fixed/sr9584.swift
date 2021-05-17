// RUN: not %target-swift-frontend -typecheck %s

struct S<N> {}

protocol P {
  associatedtype A: P = Self
  static func f(_ x: A) -> A
}

extension S: P where N: P {
  static func f<X: P>(_ x: X) -> S<X.A> where A == X, X.A == N {
    return S<X.A>()
  }
}
