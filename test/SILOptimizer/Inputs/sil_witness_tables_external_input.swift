
public struct Y {}

public protocol P {
  func doSomething() -> Y
}

public struct X : P {
  @inline(never)
  public init() {}
  @inline(never)
  public func doSomething() -> Y {
    return Y()
  }
}
