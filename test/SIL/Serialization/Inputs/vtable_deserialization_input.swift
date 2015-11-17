
public protocol P {
  func doSomething()
}

@_silgen_name("unknown") public
func unknown() -> ()

public class Y : P {
  public func doAnotherThing() {
    unknown()
  }

  public func doSomething() {
    doAnotherThing()
  }
  public init() {}
}
