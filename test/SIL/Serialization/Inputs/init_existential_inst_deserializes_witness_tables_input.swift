
public protocol P {
  func doSomething()
}

@_silgen_name("unknown") public
func unknown() -> ()

public struct X : P {
  public func doSomething() {
    unknown()
  }
  public init() {}
}

public func whatShouldIDo(_ p : P) {
  p.doSomething()
}
