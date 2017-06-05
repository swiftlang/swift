// RUN: %target-typecheck-verify-swift

// rdar://problem/23149063
protocol P0 { }

protocol P {
  associatedtype A // expected-note{{declared here}}
}

protocol Q : P {
  associatedtype A // expected-warning{{redeclaration of associated type 'A' from protocol 'P' is better expressed as a 'where' clause on the protocol}}
}

func f<T>(t: T) where T : P, T : Q, T.A : P0 { } // expected-note{{'f(t:)' previously declared here}}
// expected-warning@-1{{redundant conformance constraint 'T': 'P'}}
// expected-note@-2{{conformance constraint 'T': 'P' implied here}}

func f<T>(t: T) where T : Q, T : P, T.A : P0 { } // expected-error{{invalid redeclaration of 'f(t:)'}}
// expected-warning@-1{{redundant conformance constraint 'T': 'P'}}
// expected-note@-2{{conformance constraint 'T': 'P' implied here}}
