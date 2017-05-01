public struct S1 {
  public init(_ : Int) {}
  public func foo1() {}
  mutating public func foo2() {}
  public func foo3() {}
  public func foo4() -> Void {}
  public func foo5(x : Int, y: Int) {}

  public func foo6(x : Int, y: Int) -> Int { return 0 }
  public func foo7(x : Int, y: Int?) -> Int? { return 0 }
  public func foo8(x : Int, y: Int!) -> Int! { return 0 }
  public func foo9(x : [[Int: String?]], y: [Int]) {}
  public func foo10(x : (Int, [Int: [String: [Int]]])->String) {}
  public func foo11(x : (Int, [Int: [String: [Int]]])->String) {}
  public func foo12(x : (Int, [Int: [String: [(((Int)))]]])->String) {}
}

public class C1 {
  public class func foo1() {}
  public func foo2(_ : Int, _ b: Int) -> String { return ""}
  public weak var CIIns1 : C1?
  public var CIIns2 : C1?
  public func foo3(a : Void?) {}
}