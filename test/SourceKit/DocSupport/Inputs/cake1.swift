public protocol P1 {
  func foo1()
  var Ins : Int { get set }
  func foo2(a : Int, b: Int)
  subscript(_ a : Int) -> Int { get set }
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
