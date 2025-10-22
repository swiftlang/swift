// RUN: %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/57924

protocol P {
  associatedtype A: Sendable
}

struct S<T>: P {
  typealias A = ()
}
