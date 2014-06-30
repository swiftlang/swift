
@public struct Y {}

@public protocol P {
  func doSomething() -> Y
}

@public struct X : P {
  @public init() {}
  @public func doSomething() -> Y {
    return Y()
  }
}
