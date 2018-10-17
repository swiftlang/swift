// RUN: %target-typecheck-verify-swift

// rdar://problem/21496715
protocol P1 {
  func f() // expected-note{{protocol requires function 'f()' with type '() -> ()'}}
}

protocol Q1 {}

extension P1 where Self : Q1 {
  func f() {} // expected-note{{candidate would match if 'X1' conformed to 'Q1'}}
}

struct X1 : P1 {} // expected-error{{type 'X1' does not conform to protocol 'P1'}}

// rdar://problem/21153652
protocol P2 {
  func f()
}

struct X2 : P2 {
  func f() {}
}

extension P2 where Self : Comparable {
  func f() {}
}

// rdar://problem/19423637
protocol P3 {
  func f() // expected-note{{protocol requires function 'f()' with type '() -> ()'}}
}

extension P3 where Self : Equatable {
  func f() {} // expected-note{{candidate would match if 'X3' conformed to 'Equatable'}}
}

struct X3 : P3 {} // expected-error{{type 'X3' does not conform to protocol 'P3'}}
