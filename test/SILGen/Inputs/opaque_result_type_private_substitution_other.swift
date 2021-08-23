public protocol P {
  associatedtype A
  var v: A { get }
}

public extension P {
  func foo() -> some P {
    return self
  }
}

public struct S: P {
  public init() {}
  public var v: some P { return self }
}
