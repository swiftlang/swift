// RUN: not %target-swift-frontend -typecheck %s

protocol P {
  associatedtype M
}

struct S<M> {}

extension S: P where Self.M == P.M {}
