precedencegroup AssignmentPrecedence { assignment: true }

public enum Optional<T> {
  case none
  case some(T)
}

@_fixed_layout
public struct B {
  @inlinable
  public func amIConfused() {}
  @inlinable
  public init() {}
}

@_fixed_layout
public struct A {
  public var b : B

  @inlinable
  public init() {
    b = B()
  }

  @inlinable
  public func isBConfused() {
    b.amIConfused()
  }
}

@inlinable
public func doSomething() -> A {
  var a = A()
  return a
}
