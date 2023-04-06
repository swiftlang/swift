// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature VariadicGenerics

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// This would crash.
public struct G<T> {}

public struct GG<each T> {
  public var variables: (repeat G<each T>)

  public init() {
    fatalError()
  }
}
