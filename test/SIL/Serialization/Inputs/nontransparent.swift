precedencegroup AssignmentPrecedence { assignment: true }

public enum Optional<T> {
  case none
  case some(T)
}

@_fixed_layout
public struct B {
  @_inlineable
  public func amIConfused() {}
  @_inlineable
  public init() {}
}

@_fixed_layout
public struct A {
  public var b : B

  @_inlineable
  public init() {
    b = B()
  }

  @_inlineable
  public func isBConfused() {
    b.amIConfused()
  }
}

@_inlineable
public func doSomething() -> A {
  var a = A()
  return a
}
