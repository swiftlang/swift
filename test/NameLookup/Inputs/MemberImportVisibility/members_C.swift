@_exported import members_A
import members_B


extension X {
  public func XinC() { }

  @_spi(C)
  public func XinC_spi() { }

  public var propXinC: Bool { return true }

  public static func <>(a: Self, b: Self) -> Self { a }

  public struct NestedInC {}
}

// Members with the same names are also declared in B.
extension X {
  public init(_ x: Bool) { self.init() }
  public func ambiguous() -> Bool { return false }
  @_disfavoredOverload public func ambiguousDisfavored() -> Bool { return false }
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
  open override func overriddenMethod() {}
  public func asDerivedClassInB() -> DerivedClassInB { return self }
}

extension ProtocolInA {
  public func defaultedRequirementInC() { }
}
