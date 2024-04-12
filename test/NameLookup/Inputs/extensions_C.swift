@_exported import extensions_A
import extensions_B


extension X {
  public func XinC() { }

  public static func <>(a: Self, b: Self) -> Self { a }
}

extension Y {
  public func YinC() { }

  public static func <>(a: Self, b: Self) -> Self { a }
}
