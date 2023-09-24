// RUN: %target-typecheck-verify-swift -enable-experimental-feature TupleConformances

// REQUIRES: asserts

typealias Tuple<each T> = (repeat each T)

protocol P1 {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add type alias 'A' with underlying type '(repeat (each T).A)' for conformance}}
}

extension Tuple: P1 where repeat each T: P1 {} // expected-error {{type '(repeat each T)' does not conform to protocol 'P1'}}

protocol P2 {
  associatedtype A = Int // expected-note {{default type 'Int' for associated type 'A' (from protocol 'P2') is unsuitable for tuple conformance; the associated type requirement must be fulfilled by a type alias with underlying type '(repeat (each T).A)'}}
}

extension Tuple: P2 where repeat each T: P2 {} // expected-error {{type '(repeat each T)' does not conform to protocol 'P2'}}

protocol P3 {
  associatedtype A // expected-note {{unable to infer associated type 'A' for protocol 'P3'}}
  func f() -> A
}

extension Tuple: P3 where repeat each T: P3 { // expected-error {{type '(repeat each T)' does not conform to protocol 'P3'}}
  func f() -> Int {} // expected-note {{cannot infer 'A' = 'Int' in tuple conformance because the associated type requirement must be fulfilled by a type alias with underlying type '(repeat (each T).A)'}}
}
