// RUN: %target-swift-frontend -typecheck %s

protocol P {
  associatedtype A: Sendable
}

struct S<T>: P {
  typealias A = ()
}
