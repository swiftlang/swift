
public protocol P {
  func doSomething()
}

@_silgen_name("unknown") public
func unknown() -> ()

@_fixed_layout
public class Y : P {
  @inlinable
  public func doAnotherThing() {
    unknown()
  }

  @inlinable
  public func doSomething() {
    doAnotherThing()
  }
  @inlinable
  public init() {}
}
