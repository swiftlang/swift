// RUN: %target-typecheck-verify-swift

protocol P0 { }
protocol P0b { }
class Class {}

struct X0 : P0 { }

struct X1 { }

protocol P1 {
  associatedtype A // expected-note 2{{'A' declared here}}
  associatedtype A2 // expected-note {{'A2' declared here}}
  associatedtype A3 // expected-note {{'A3' declared here}}
  associatedtype A4 // expected-note {{'A4' declared here}}
  associatedtype A5 // expected-note {{'A5' declared here}}
}

/*
A typealias or constrained associated type that overrides an associated type
from an inherited protocol should be written as a same-type requirement...
*/

protocol P2a : P1 {
  typealias A = X1 // expected-warning{{typealias overriding associated type 'A' from protocol 'P1' is better expressed as same-type constraint on the protocol}}{{-1:18-18= where A == X1}}{{3-20=}}
}

// A redeclaration of an associated type that adds type/layout requirements
// should be written via a where clause.
protocol P2b : P1 {
  associatedtype A: P0, P0b // expected-warning{{redeclaration of associated type 'A' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{-1:18-18= where A: P0, A: P0b}}{{3-29=}}
}

protocol P2c : P1 {
  associatedtype A2: P0, P0b where A2.A == Never, A2: P1 // expected-warning{{redeclaration of associated type 'A2' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{-1:18-18= where A2: P0, A2: P0b, A2.A == Never, A2 : P1}}{{3-58=}}
}

protocol P2d : P1 {
  associatedtype A3 where A3: P0 // expected-warning{{redeclaration of associated type 'A3' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{-1:18-18= where A3 : P0}}{{3-34=}}
}

protocol P2e : P1 {
  // expected-warning@+1 {{redeclaration of associated type 'A4' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{-2:18-18= where A4: P0, A4 : Collection, A4.Element == A4.Index, A4.SubSequence == A4}}{{3-+2:51=}}
  associatedtype A4: P0 where A4: Collection,
                              A4.Element == A4.Index,
                              A4.SubSequence == A4
}

protocol P2f : P1 {
  // expected-warning@+1 {{redeclaration of associated type 'A5' from protocol 'P1' is better expressed as a 'where' clause on the protocol}}{{-2:18-18= where A5: P0b & Class, A5 : Collection, A5.Element : Collection}}{{3-+1:62=}}
  associatedtype A5: P0b & Class where A5: Collection,
                                       A5.Element: Collection
}

/*
 ... unless it's a defaulted associated type.
*/
protocol P2g : P1 {
  associatedtype A: P0 = X0   // note: no warning
}

protocol P3a: P1 {
  associatedtype B // expected-note{{'B' declared here}}
  associatedtype B2 // expected-note{{'B2' declared here}}
}
protocol P3b: P3a where A: P0 {
  typealias B = X1 // expected-warning{{typealias overriding associated type 'B' from protocol 'P3a' is better expressed as same-type constraint on the protocol}}{{-1:30-30=, B == X1}}{{3-20=}}
  associatedtype B2: P3b // expected-warning{{redeclaration of associated type 'B2' from protocol 'P3a' is better expressed as a 'where' clause on the protocol}}{{-2:30-30=, B2: P3b}} {{3-26=}}
}


