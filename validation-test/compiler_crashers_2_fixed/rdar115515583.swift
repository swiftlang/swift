// RUN: %target-swift-frontend -emit-ir %s

public protocol P {
  associatedtype A: P
}

public struct S {
  public static func foo<each T: P>(t: repeat each T) -> some P {
    return G<(repeat each T)>((repeat each t))
  }
}

public struct G<T>: P {
  public typealias A = Self

  public init(_: T) {}
}
