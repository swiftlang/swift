// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference

public protocol P {
  associatedtype A = Never
}

public struct G<A>: P {}

public struct ConcreteA {}

public protocol Q: P where Self.A == ConcreteA {}

extension G: Q where A == ConcreteA {}
