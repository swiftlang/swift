// RUN: %target-typecheck-verify-swift

class C { 
  func f() {}
}

class D : C { 
}

class E { }

protocol P {
  associatedtype Assoc : C // expected-note{{unable to infer associated type 'Assoc' for protocol 'P'}}
  func getAssoc() -> Assoc
}

struct X1 : P {
  func getAssoc() -> D { return D() }
}

struct X2 : P { // expected-error{{type 'X2' does not conform to protocol 'P'}}
  func getAssoc() -> E { return E() } // expected-note{{candidate would match and infer 'Assoc' = 'E' if 'E' inherited from 'C'}}
}

func testP<T:P>(_ t: T) {
  _ = t.getAssoc() as C
  t.getAssoc().f()
}

func callTestP(_ x1: X1) {
  testP(x1)
}
