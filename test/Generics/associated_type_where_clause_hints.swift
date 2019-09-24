// RUN: %target-typecheck-verify-swift

protocol P0 { }
protocol P0b { }

struct X0 : P0 { }

struct X1 { }

protocol P1 {
  associatedtype A // expected-note 2{{'A' declared here}}
}

// A typealias in a subprotocol should be written as a same-type
// requirement.
protocol P2 : P1 {
  typealias A = X1 // expected-warning{{typealias overriding associated type 'A' from protocol 'P1' is better expressed as same-type constraint on the protocol}}{{17-17= where A == X1}}{{3-20=}}
}

// A redeclaration of an associated type that adds type/layout requirements
// should be written via a where clause.
protocol P3a : P1 {
  associatedtype A: P0, P0b // expected-warning{{redeclaration of associated type 'A' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{18-18= where A: P0, A: P0b}}{{3-29=}}
}

// ... unless it has adds a default type witness
protocol P3b : P1 {
  associatedtype A: P0 = X0   // note: no warning
}

protocol P4: P1 {
  associatedtype B // expected-note{{'B' declared here}}
}

protocol P5: P4 where A: P0 {
  typealias B = X1 // expected-warning{{typealias overriding associated type 'B' from protocol 'P4' is better expressed as same-type constraint on the protocol}}{{28-28=, B == X1}}{{3-20=}}
}


