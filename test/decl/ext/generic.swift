// RUN: %swift -parse %s -verify

protocol P1 { }
protocol P2 : P1 { }

struct X<T : P1, U : P2, V> { }

// FIXME: This will be okay.
extension X<T : P1, U : P2, V> { // expected-error{{generic arguments are not allowed on an extension}}
}
