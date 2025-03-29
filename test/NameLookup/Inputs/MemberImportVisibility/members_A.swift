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
  public init() {}
  open func methodInA() {}
}

public protocol ProtocolInA {
  func defaultedRequirementInA()
  func defaultedRequirementInB()
  func defaultedRequirementInC()
}

extension ProtocolInA {
  public func defaultedRequirementInA() { }
}
