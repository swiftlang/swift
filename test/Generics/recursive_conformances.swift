// RUN: %target-typecheck-verify-swift

protocol P1 {
  associatedtype T : P1
}

protocol P2 {
  associatedtype T : P3
}

protocol P3 {
  associatedtype T : P1
}

struct S<T : P1 & P2> {}

protocol Base {
  associatedtype T : Base // expected-note {{'T' declared here}}
}

protocol Derived1 : Base {
  associatedtype T : Derived1 // expected-warning {{redeclaration of associated type 'T' from protocol 'Base' is better expressed as a 'where' clause on the protocol}}
}

protocol Derived2 : Base where T : Derived2 {}
