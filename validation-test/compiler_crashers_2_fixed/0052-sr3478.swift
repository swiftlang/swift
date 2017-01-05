// RUN: %target-swift-frontend %s -typecheck

protocol P {
  associatedtype A
}

struct S : P {
  typealias A = Gen<S>
}

struct Gen<T: P> {}
