import members_A


extension X {
  public func XinB() { }
  package func XinB_package() { }

  @_spi(B)
  public func XinB_spi() { }

  public var propXinB: Bool { return true }
  package var propXinB_package: Bool { return true }

  public func shadowedByMemberOnXinB() { }
  public static func shadowedByStaticMemberOnXinB() { }

  public static var max: Int { return Int.min }

  public static func >>>(a: Self, b: Self) -> Self { b }

  public struct NestedInB {}
  package struct NestedInB_package {}
  public protocol ProtoNestedInB {}
}

// Members with the same names are also declared in C.
extension X {
  public init(_ x: Int) { self.init() }
  public func ambiguous() -> Int { return 1 }
  public func ambiguousDisfavored() -> Int { return 1 }
  public var ambiguousProp: Bool { return false }
  public struct AmbiguousNestedType { }
}

extension Y {
  public func YinB() { }

  public static func >>>(a: Self, b: Self) -> Self { b }
}

extension P where Self == Z {
  public static var zInB: Z { Z() }
  public static var zAmbiguous: Z { Z() }
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
  open override func overriddenInBMethod() {}
}

extension ProtocolInA2 {
  public func defaultedRequirementInB() { }
}

extension ProtocolInA3 {
  public func defaultedRequirementInBAndC() { }
}

extension StructInA1 {
  public struct WitnessedInB { }
}

extension StructInA2 {
  public var hashValue: Int { 0 }
}

public protocol ProtocolInB1 {
  func defaultedRequirementInB()
}

extension ProtocolInB1 {
  public func defaultedRequirementInB() { }
}

public protocol ProtocolInB2 {
  func defaultedRequirementInC()
}

public struct EquatableInB: Equatable {
  public static func ==(_: EquatableInB, _: EquatableInB) -> Bool {
    false
  }
}
