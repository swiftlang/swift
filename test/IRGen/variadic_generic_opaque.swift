// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature VariadicGenerics -disable-availability-checking

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// FIXME: Add more tests

public protocol P {}

public struct G<each T>: P {}

public func returnsG<each T>(_ t: repeat each T) -> some P {
  return G<repeat each T>()
}
