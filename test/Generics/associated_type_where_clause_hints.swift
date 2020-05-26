// RUN: %target-typecheck-verify-swift

protocol P0 { }
protocol P0b { }

struct X0 : P0 { }

struct X1 { }

protocol P1 {
  associatedtype A // expected-note 2{{'A' declared here}}
  associatedtype A2 // expected-note {{'A2' declared here}}
  associatedtype A3 // expected-note {{'A3' declared here}}
  associatedtype A4 // expected-note {{'A4' declared here}}
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
  associatedtype A2: P0, P0b where A2.A == Never, A2: P1 // expected-warning{{redeclaration of associated type 'A2' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{18-18= where A2: P0, A2: P0b, A2.A == Never, A2 : P1}}{{3-58=}}
  associatedtype A3 where A3: P0 // expected-warning{{redeclaration of associated type 'A3' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{18-18= where A3 : P0}}{{3-34=}}

  // expected-warning@+1 {{redeclaration of associated type 'A4' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{18-18= where A4: P0, A4 : Collection, A4.Element == A4.Index, A4.SubSequence == A4}}{{3-52=}}
  associatedtype A4: P0 where A4: Collection,
                              A4.Element == A4.Index,
                              A4.SubSequence == A4 // {{3-52=}} is this line, so it is correct.
}

// ... unless it has adds a default type witness
protocol P3b : P1 {
  associatedtype A: P0 = X0   // note: no warning
}

protocol P4: P1 {
  associatedtype B // expected-note{{'B' declared here}}
  associatedtype B2 // expected-note{{'B2' declared here}}
}

protocol P5: P4 where A: P0 {
  typealias B = X1 // expected-warning{{typealias overriding associated type 'B' from protocol 'P4' is better expressed as same-type constraint on the protocol}}{{28-28=, B == X1}}{{3-20=}}
  associatedtype B2: P5 // expected-warning{{redeclaration of associated type 'B2' from protocol 'P4' is better expressed as a 'where' clause on the protocol}}{{28-28=, B2: P5}} {{3-25=}}
}


