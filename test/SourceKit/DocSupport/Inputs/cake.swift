public protocol Prot {
  typealias Element
  var p : Int { get }
  func foo()
}

public class C1 : Prot {
  public typealias Element = Int
  public var p : Int = 0
  public func foo() {}

  public subscript(index: Int) -> Int { return 0 }
  public subscript(index i: Float) -> Int { return 0 }
}

public func genfoo<T1 : Prot, T2 : C1 where T1.Element == Int, T2.Element == T1.Element>(x ix: T1, y iy: T2) {}

public extension Prot where Self.Element == Int {
  final func extfoo() {}
}

public enum MyEnum : Int {
  case Blah
}

protocol Prot1 {}

typealias C1Alias = C1

extension C1Alias : Prot1 {}
