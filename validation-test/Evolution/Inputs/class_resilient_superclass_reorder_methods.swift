public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if BEFORE
open class Base {
  public init() {}
  open func firstMethod() -> Int {
    return 1
  }
  open func secondMethod() -> Int {
    return 2
  }
  open func callOverriddenMethods() -> Int {
    return firstMethod() * 10 + secondMethod()
  }
}
#else
open class Base {
  public init() {}
  open func secondMethod() -> Int {
    return 2
  }
  open func firstMethod() -> Int {
    return 1
  }
  open func callOverriddenMethods() -> Int {
    return firstMethod() * 10 + secondMethod()
  }
}
#endif

public class Derived : Base {
  public override func firstMethod() -> Int {
    return 10
  }

  public override func secondMethod() -> Int {
    return 20
  }
}
