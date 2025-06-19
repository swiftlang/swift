public struct X {
  public init() {}
}

public protocol P { }

public struct Y<T> { }

extension Y: P where T: P { }

public struct Z: P { }

infix operator <<<
infix operator >>>
infix operator <>

extension X {
  public func XinA() { }

  @_spi(A)
  public func XinA_spi() { }

  public var propXinA: Bool { return true }

  public static func <<<(a: Self, b: Self) -> Self { a }

  public struct NestedInA {}
}

extension Y {
  public func YinA() { }

  public static func <<<(a: Self, b: Self) -> Self { a }
}

public enum EnumInA {
  case caseInA
}

open class BaseClassInA {
  open func methodInA() {}
  open func overriddenMethod() {}
}

public protocol ProtocolInA {
  func defaultedRequirementInA()
  func defaultedRequirementInB()
  func defaultedRequirementInC()
}

extension ProtocolInA {
  public func defaultedRequirementInA() { }
}
