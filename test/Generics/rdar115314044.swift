// RUN: %target-typecheck-verify-swift

public protocol P {}

public protocol Q {
  associatedtype A: P
  func f(_: A)
}

open class Parent<C: P>: Q {
  public func f(_: C) {}
}

final class Child: Parent<Child.Nested> {
  struct Nested: P {}
}
