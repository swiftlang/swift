public protocol P {
  associatedtype A: Q

  func foo() -> A
}

public protocol Q {}

@_transparent public func f(p: any P) -> any Q {
  return p.foo()
}
