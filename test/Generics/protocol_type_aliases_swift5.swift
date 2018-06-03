// RUN: %target-typecheck-verify-swift -typecheck %s -verify -swift-version 5

protocol P1 {
  typealias A = Int
  // expected-error@-1 {{typealias must not be declared within a protocol; move to an extension or use 'associatedtype' to define an associated type requirement}} {{3-12=associatedtype}}
}
protocol P2 : P1 {
  associatedtype A
}

protocol P3 {
  associatedtype A // expected-note 3 {{'A' declared here}}
  associatedtype B
}

// Complain and since we have 'A' from 'P3', provide some options the user might have had in mind
protocol P4: P3 {
  typealias A = Int // expected-error {{typealias must not be declared within a protocol}}
  // expected-note@-1 {{use 'associatedtype' to specify a default value for 'A' from protocol 'P3'}} {{3-12=associatedtype}}
  // expected-note@-2 {{use a same-type constraint on the protocol}} {{16-16= where A == Int}} {{3-21=}}
}
protocol P5: P3 where B == Int {
  typealias A = Bool // expected-error {{typealias must not be declared within a protocol}}
  // expected-note@-1 {{use 'associatedtype' to specify a default value for 'A' from protocol 'P3'}} {{3-12=associatedtype}}
  // expected-note@-2 {{use a same-type constraint on the protocol}} {{31-31=, A == Bool}} {{3-22=}}
}

// Nothing changes here
protocol P6: P3 {
  associatedtype A: P1
  // expected-warning@-1 {{redeclaration of associated type 'A' from protocol 'P3' is better expressed as a 'where' clause on the protocol}}
}
