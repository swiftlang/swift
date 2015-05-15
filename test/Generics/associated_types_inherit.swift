// RUN: %target-parse-verify-swift

class C { 
  func f() {}
}

class D : C { 
}

class E { }

protocol P {
  typealias Assoc : C
  func getAssoc() -> Assoc // expected-note{{protocol requires function 'getAssoc()' with type '() -> E'}}
}

struct X1 : P {
  func getAssoc() -> D { return D() }
}

struct X2 : P { // expected-error{{type 'X2' does not conform to protocol 'P'}}
  func getAssoc() -> E { return E() } // expected-note{{candidate has non-matching type '() -> E'}}
}

func testP<T:P>(t: T) {
  _ = t.getAssoc() as C
  t.getAssoc().f()
}

func callTestP(x1: X1) {
  testP(x1)
}
