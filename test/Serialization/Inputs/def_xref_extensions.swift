public struct Outer {
  public struct InterestingValue {}
}

public struct Other {
  public struct InterestingValue {}
}

public struct InterestingValue {}

extension Outer.InterestingValue {
  public static func foo() {}
}
extension Other.InterestingValue {
  public static func bar() {}
}
extension InterestingValue {
  public static func bar() {}
}

#if EXTRA
// Make sure that adding more of these doesn't change anything.
extension Other.InterestingValue {
  public static func baz() {}
}
extension InterestingValue {
  public static func baz() {}
}
#endif
