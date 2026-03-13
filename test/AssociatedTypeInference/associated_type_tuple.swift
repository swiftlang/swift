// RUN: %target-typecheck-verify-swift -enable-experimental-feature TupleConformances

// REQUIRES: swift_feature_TupleConformances

typealias Tuple<each T> = (repeat each T)

protocol P1 {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
}

extension Tuple: P1 where repeat each T: P1 {} // expected-error {{type '(repeat each T)' does not conform to protocol 'P1'}} expected-note {{add stubs for conformance}}

protocol P2 {
  associatedtype B = Int
}

extension Tuple: P2 where repeat each T: P2 {} // expected-error {{type '(repeat each T)' does not conform to protocol 'P2'}}
// expected-note@-1 {{possibly intended match 'Int' is unsuitable for tuple conformance; the associated type requirement must be fulfilled by a type alias with underlying type '(repeat (each T).B)'}}

protocol P3 {
  associatedtype C
  func f() -> C
}

extension Tuple: P3 where repeat each T: P3 { // expected-error {{type '(repeat each T)' does not conform to protocol 'P3'}}
  // expected-note@-1 {{possibly intended match 'Int' is unsuitable for tuple conformance; the associated type requirement must be fulfilled by a type alias with underlying type '(repeat (each T).C)'}}
  func f() -> Int {}
}
