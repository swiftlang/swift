public struct S1 {
  public init(_ : Double) {}
  mutating public func foo1() {}
  mutating public func foo2() {}
  public static func foo3() {}
  public func foo4() {}
  public func foo5(x : Int, y: Int, z: Int) {}

  public func foo6(x : Int, y: Int?) -> Int? { return 0 }
  public func foo7(x : Int, y: Int) -> Int { return 0 }
  public func foo8(x : Int, y: Int?) -> Int? { return 0 }
  public func foo9(x : [[Int: String]], y: [Int?]) {}
  public func foo10(x : (Int, [Int: [String: [Int?]]])->String) {}
  public func foo11(x : (Int, [Int: Int])->String) {}
  public func foo12(x : (Int, [Int: [String: [(((String)))]]])->String) {}
}

public class C1 {
  public func foo1() {}
  public func foo2(_ : ()->(), _ b : String) -> Int { return 0 }
  public var CIIns1 : C1?
  public weak var CIIns2 : C1?
  public func foo3(a : ()?) {}
}