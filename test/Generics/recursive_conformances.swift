// RUN: %target-typecheck-verify-swift -enable-requirement-machine

// Make sure the requirement machine can compute a confluent
// completion in examples where we merge two associated types
// with the same name, both having recursive conformance
// requirements.

// Merged two cycles of length 1:
//
// P1 -> P1 -> P1 -> ...
// P2 -> P2 -> P2 -> ...
protocol P1 {
  associatedtype T : P1
}

protocol P2 {
  associatedtype T : P2
}

struct S<T : P1 & P2> {}

// Merged a cycle of length 2 with a cycle of length 3
//
// P1a -> P1b -> P1a -> P1b -> P1a -> P1b -> ...
// P2a -> P2b -> P2c -> P2a -> P2b -> P2c -> ...
protocol P1a {
  associatedtype T : P1b
}

protocol P1b {
  associatedtype T : P1a
}

protocol P2a {
  associatedtype T : P2b
}

protocol P2b {
  associatedtype T : P2c
}

protocol P2c {
  associatedtype T : P2a
}

struct SS<T : P1a & P2a> {}

// Merged two cycles of length 1 via an inherited associated type
protocol Base {
  associatedtype T : Base // expected-note {{'T' declared here}}
}

// Base     -> Base     -> Base     -> ...
// Derived1 -> Derived1 -> Derived1 -> ...
protocol Derived1 : Base {
  associatedtype T : Derived1 // expected-warning {{redeclaration of associated type 'T' from protocol 'Base' is better expressed as a 'where' clause on the protocol}}
}

// Base     -> Base     -> Base     -> ...
// Derived2 -> Derived2 -> Derived2 -> ...
protocol Derived2 : Base where T : Derived2 {}