public protocol P {
  func foo()
}

public protocol Q: P {
  func bar()
}

public struct S {}

extension S: P {
  public func foo() {}
}

extension S: Q {
  public func bar() {}
}
