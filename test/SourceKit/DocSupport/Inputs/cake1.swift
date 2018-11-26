public protocol P1 {
  func foo1()
  var Ins : Int { get set }
  func foo2(a : Int, b: Int)
  subscript(_ a : Int) -> Int { get set }
  func fooConstraint()
}

public protocol P2 : P1 {
  func bar1()
  func bar2()
}

public extension P2 {
  func foo1() { }
  var Ins : Int { get { return 1 } set {}}
  func foo2(a : Int, b: Int) {}
  subscript(_ a : Int) -> Int { get {return 1} set {} }
}

public protocol P3 {
  func p3Required()
}

public extension P2 where Self : P3 {
  func fooConstraint() {}
}

public extension Dictionary.Keys {
  public func foo() {}
}

public extension Dictionary.Keys where Key: P1 {
  public func bar() {}
}

public protocol InitProto {
  init(x: Int)
}
extension InitProto {
  // This initializer is marked as 'CtorInitializerKind::Convenience'.
  public init() { self = Self(x: 0) }
}

public struct InitStructImpl : InitProto {
  public init(x: Int) {}
}

public class InitClassImpl : InitProto {
  public required init(x: Int) {}
}
