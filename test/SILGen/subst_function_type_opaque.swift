// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s

public protocol P {}

extension P {
}

public struct S: P {
  public func callAsFunction<V>(_: () -> V) { }
}

public func f() -> some P {
  S()
}

public struct G<T: P>: P {
  public init(_: () -> T) {}
}

S() {
  return G { return f() }
}

