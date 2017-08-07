public struct S1 {
  public static func foo1() {}
  mutating public func foo2() {}
  internal func foo3() {}
  private func foo4() {}
  fileprivate func foo5() {}
  public func foo6() -> Void {}
}

public class C0 {}

public class C1: C0 {
	open class func foo1() {}
	public weak var Ins : C1?
	public unowned var Ins2 : C1 = C1()
}
