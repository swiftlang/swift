// RUN: %target-typecheck-verify-swift

protocol P {}

struct X: P {}

protocol Q {
  typealias A = X
}

protocol R {
  associatedtype A: Q
  associatedtype B: P
}

protocol S {}

extension S where Self: R {
  typealias B = Self.A.A
}

typealias T<A: Q> = G<A>.B

struct G<A: Q>: R, S {
  typealias A = A
}
