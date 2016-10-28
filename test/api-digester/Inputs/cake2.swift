public struct S1 {
  public init(_ : Double) {}
  mutating public func foo1() {}
  mutating public func foo2() {}
  public static func foo3() {}
}

public class C1 {
  public func foo1() {}
  public func foo2(_ : ()->()) {}
}