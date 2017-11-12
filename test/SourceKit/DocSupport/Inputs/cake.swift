public protocol Prot {
  associatedtype Element
  var p : Int { get }
  func foo()
  func foo1()
}

public class C1 : Prot {
  public typealias Element = Int
  public var p : Int = 0
  public func foo() {}

  public subscript(index: Int) -> Int { return 0 }
  public subscript(index i: Float) -> Int { return 0 }
}

public func genfoo<T1 : Prot, T2 : C1>(x ix: T1, y iy: T2) where T1.Element == Int, T2.Element == T1.Element {}

public extension Prot where Self.Element == Int {
  func extfoo() {}
}

public enum MyEnum : Int {
  case Blah
}

protocol Prot1 {}

typealias C1Alias = C1

extension C1Alias : Prot1 {}

public extension Prot {
  public func foo1() {}
}

public struct S1 {
  public enum SE {
  case a
  case b
  case c
  }
}
public extension S1 {
  public func foo1() {}
  public struct S2 {
    public let b = 1
  }
}

@objc
public protocol P2 {
  @objc optional func foo1()
}

public protocol P3 {
  associatedtype T
}

public struct S2 : P3 {
  public typealias T = S2
}

public extension C1 {
  public enum C1Cases : Int {
  case case1
  }
}

public class C2 : C1 {
  public func C2foo() {}
}

public extension Prot {
  subscript(index: Int) -> Int { return 0 }
}

public protocol P4 {}

extension C1 : P4 {
  public func C1foo() {}
  public struct C1S1{
    public func C1S1foo(a : P4) {}
  }
}
