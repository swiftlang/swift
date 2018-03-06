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
