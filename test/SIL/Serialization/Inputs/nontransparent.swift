precedencegroup AssignmentPrecedence { assignment: true }

public enum Optional<T> {
  case none
  case some(T)
}

public struct B {
  public func amIConfused() {}
}

public struct A {
  public var b : B

  public init() {
    b = B()
  }

  public func isBConfused() {
    b.amIConfused()
  }
}

public func doSomething() -> A {
  var a = A()
  return a
}
