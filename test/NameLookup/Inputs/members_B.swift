import members_A


extension X {
  public func XinB() { }

  public static func >>>(a: Self, b: Self) -> Self { b }
}

extension Y {
  public func YinB() { }

  public static func >>>(a: Self, b: Self) -> Self { b }
}

public enum EnumInB {
  case caseInB
}
