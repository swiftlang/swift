// RUN: %target-swift-frontend -emit-ir -g %s

public protocol P {
  associatedtype Element
}

public struct G<T, U> {
  public struct Nested {}

  public init(_: (Nested) -> ()) {}
}

public extension P {
    var values: G<Element, Int> {
        G<Element, Int> { x in }
    }
}
