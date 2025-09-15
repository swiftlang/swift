// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple

// This would crash.
public struct G<T> {}

public struct GG<each T> {
  public var variables: (repeat G<each T>)

  public init() {
    fatalError()
  }
}
