// RUN: %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/46066

protocol P {
  associatedtype A
}

struct S : P {
  typealias A = Gen<S>
}

struct Gen<T: P> {}
