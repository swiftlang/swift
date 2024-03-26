// RUN: %target-typecheck-verify-swift

public protocol P {
  associatedtype A = Never
}

public struct G<A>: P {}

public struct ConcreteA {}

public protocol Q: P where Self.A == ConcreteA {}

extension G: Q where A == ConcreteA {}
