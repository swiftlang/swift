// RUN: %target-typecheck-verify-swift

class A {}
class B: A {}

protocol P {
  func f1(o: Int?)
  func f2() -> Int?
}

protocol P1 {
  var a: A { get set }
  func f() -> A
}

struct S : P1 {
  func f() -> B {
    return B()
  }
  var a = B()
}

extension S: P {
  func f1(o: Int) {
    print(o)
  }
  func f2() -> Int {
    return 1
  }
}

class C<G: A>: P1 {
  func f() -> G {
    return B() as! G
  }
  var a: G = B() as! G
}


protocol P2 {
  func f(_:(Int?)->())
}
struct X : P2 {
  func f(_:(Int)->()) {}
}

protocol Q {
  var i: Int? { get }
}
struct Y : Q {
  var i: Int = 0
}

protocol A1 {}
protocol B2 {}

protocol HasA1 {
  var a: A1 { get }
}

struct Baz: HasA1 {
  var a: A1 & B2
}

