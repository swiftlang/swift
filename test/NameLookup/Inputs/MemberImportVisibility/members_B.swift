import members_A


extension X {
  public func XinB() { }
  package func XinB_package() { }

  public var propXinB: Bool { return true }
  package var propXinB_package: Bool { return true }

  public static func >>>(a: Self, b: Self) -> Self { b }

  public struct NestedInB {}
  package struct NestedInB_package {}
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
}

extension ProtocolInA {
  public func defaultedRequirementInB() { }
}
