// All of this is required in order to produce materializeForSet
// declarations for A's properties.
precedencegroup AssignmentPrecedence { assignment: true }

public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}
public enum Optional<T> : ExpressibleByNilLiteral {
  case none
  case some(T)

  public init(nilLiteral: ()) { self = .none }
}

public struct Y {}

public struct X<U> {
  public var a : U

  public init(_a : U) {
    a = _a
  }

  public func doneSomething() {}
}

public class A {
  public var y : Y
  public var x : X<Y>

  public init() {
    y = Y()
    x = X<Y>(_a: y)
  }

  public func doSomething() {
    x.doneSomething()
  }
}
