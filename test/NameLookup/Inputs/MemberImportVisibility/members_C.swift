@_exported import members_A
import members_B


extension X {
  public func XinC() { }

  public var propXinC: Bool { return true }

  public static func <>(a: Self, b: Self) -> Self { a }

  public struct NestedInC {}
}

extension Y {
  public func YinC() { }

  public static func <>(a: Self, b: Self) -> Self { a }
}

public enum EnumInC {
  case caseInC
}

open class DerivedClassInC: DerivedClassInB {
  open func methodInC() {}
}

extension ProtocolInA {
  public func defaultedRequirementInC() { }
}
