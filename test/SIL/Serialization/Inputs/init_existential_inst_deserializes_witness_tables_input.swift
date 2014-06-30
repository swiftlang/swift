
@public protocol P {
  func doSomething()
}

@asmname("unknown") @public
func unknown() -> ()

@public struct X : P {
  @public func doSomething() {
    unknown()
  }
  @public init() {}
}

@public func whatShouldIDo(p : P) {
  p.doSomething()
}
