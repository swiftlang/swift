// RUN: %swift -parse %s -verify

protocol P1 {
  subscript (i: Int) -> Int // expected-note{{protocol requires subscript operator with type '(i: Int) -> Int'}}
}

class C1 : P1 {
  subscript (i: Int) -> Int {
  get:
    return i

  set:
  }
}

struct S1 : P1 {
  subscript (i: Int) -> Int {
  get:
    return i

  set:
  }
}

struct S1Error : P1 { // expected-error{{type 'S1Error' does not conform to protocol 'P1'}}
  subscript (i: Int) -> Double { // expected-note{{candidate has non-matching type '(i: Int) -> Double'}}
  get:
    return Double(i)

  set:
  }
}
