// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature AssociatedTypeDisambiguation \
// RUN:   -disable-experimental-parser-round-trip

// REQUIRES: swift_feature_AssociatedTypeDisambiguation

// Error cases for protocol-qualified witness aliases.

struct X {}
struct Y {}

// Two qualified witnesses for the *same* protocol with different types: the
// witness is ambiguous, so the conformance is unsatisfied.
protocol A1 { associatedtype Item } // expected-note {{multiple matching types named 'Item'}}
struct Dup: A1 { // expected-error {{type 'Dup' does not conform to protocol 'A1'}}
  typealias A1.Item = X // expected-note {{possibly intended match}}
  typealias A1.Item = Y // expected-note {{possibly intended match}}
}

// Qualifying with a protocol the type does not conform to: the alias does not
// witness 'A2', which is left unsatisfied.
protocol A2 { associatedtype Item } // expected-note {{protocol requires nested type 'Item'}}
protocol B2 { associatedtype Item }
struct Misqualified: A2 { // expected-error {{type 'Misqualified' does not conform to protocol 'A2'}} expected-note {{add stubs for conformance}}
  typealias B2.Item = X
}
