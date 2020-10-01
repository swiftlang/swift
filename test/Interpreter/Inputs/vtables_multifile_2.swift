
open class Base {
  public init() {}

  fileprivate func privateMethod() -> Int {
    return 1
  }
}

open class Derived : Base {
  open override func privateMethod() -> Int {
    return super.privateMethod() + 1
  }
}

public final class FinalDerived : Base {
  public override func privateMethod() -> Int {
    return super.privateMethod() + 1
  }
}

public func callBaseMethod(_ b: Base) -> Int {
  return b.privateMethod()
}

public func callDerivedMethod(_ d: Derived) -> Int {
  return d.privateMethod()
}
