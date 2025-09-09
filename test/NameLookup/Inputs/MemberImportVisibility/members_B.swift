import members_A


extension X {
  public func XinB() { }
  package func XinB_package() { }

  @_spi(B)
  public func XinB_spi() { }

  public var propXinB: Bool { return true }
  package var propXinB_package: Bool { return true }

  public static func >>>(a: Self, b: Self) -> Self { b }

  public struct NestedInB {}
  package struct NestedInB_package {}
}

// Members with the same names are also declared in C.
extension X {
  public init(_ x: Int) { self.init() }
  public func ambiguous() -> Int { return 1 }
  public func ambiguousDisfavored() -> Int { return 1 }
}

extension Y {
  public func YinB() { }

  public static func >>>(a: Self, b: Self) -> Self { b }
}

public enum EnumInB {
  case caseInB
}

package enum EnumInB_package {
  case caseInB
}

open class DerivedClassInB: BaseClassInA {
  open func methodInB() {}
  open override func overriddenMethod() {}
}

extension ProtocolInA {
  public func defaultedRequirementInB() { }
}
