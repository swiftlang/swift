// RUN: %target-parse-verify-swift

class C { 
  func f() {}
}

class D : C { 
}

class E { }

protocol P { // expected-note{{requirement specified as '`Self`.Assoc' : 'C' [with Self = X2]}}
  typealias Assoc : C
  func getAssoc() -> Assoc
}

struct X1 : P {
  func getAssoc() -> D { return D() }
}

struct X2 : P { // expected-error{{'P' requires that 'E' inherit from 'C'}}
  func getAssoc() -> E { return E() }
}

func testP<T:P>(t: T) {
  _ = t.getAssoc() as C
  t.getAssoc().f()
}

func callTestP(x1: X1) {
  testP(x1)
}
