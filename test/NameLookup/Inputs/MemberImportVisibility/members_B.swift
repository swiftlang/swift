import members_A


extension X {
  public func XinB() { }

  public var propXinB: Bool { return true }

  public static func >>>(a: Self, b: Self) -> Self { b }

  public struct NestedInB {}
}

extension Y {
  public func YinB() { }

  public static func >>>(a: Self, b: Self) -> Self { b }
}

public enum EnumInB {
  case caseInB
}
