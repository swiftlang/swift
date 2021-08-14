public protocol P {
  func foo()
}

public protocol Q: P {
  func bar()
}

public struct S {}

extension S: P {
  public func foo() {}
  public func bar() {}
}

extension S: Q {

}
