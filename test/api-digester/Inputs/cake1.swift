public struct S1 {
  public init(_ : Int) {}
  public func foo1() {}
  mutating public func foo2() {}
  public func foo3() {}
}

public class C1 {
  public class func foo1() {}
  public func foo2(_ : Int) {}
}