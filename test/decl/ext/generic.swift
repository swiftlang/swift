// RUN: %swift -parse %s -verify

protocol P1 { }
protocol P2 : P1 { }
protocol P3 { }

struct X<T : P1, U : P2, V> { }

// Okay: exact match.
extension X<T : P1, U : P2, V> { } // expected-error{{generic arguments are not allowed on an extension}}

// Okay: infer missing requirements
extension X<T, U, V> { } // expected-error{{generic arguments are not allowed on an extension}}
extension X<T : P1, U, V> { } // expected-error{{generic arguments are not allowed on an extension}}
extension X<T, U : P2, V> { } // expected-error{{generic arguments are not allowed on an extension}}

// Bad: extra requirements.
extension X<T : P2, U, V> { } // expected-error{{extension of generic type 'X' cannot add requirements}}
extension X<A, B, C: P3> { } // expected-error{{extension of generic type 'X' cannot add requirements}}

// Bad: wrong number of generic parameters.
extension X<T> { } // expected-error{{extension of generic type 'X' has too few generic parameters (have 1, expected 3)}}
extension X<A, B, C, D> { } // expected-error{{extension of generic type 'X' has too many generic parameters (have 4, expected 3)}}
