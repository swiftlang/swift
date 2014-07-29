// RUN: %swift -parse %s -verify

protocol P1 { typealias AssocType }
protocol P2 : P1 { }
protocol P3 { }

struct X<T : P1, U : P2, V> { 
  struct Inner<A, B : P3> { }

  struct NonGenericInner { } // expected-note{{extended type 'X<T, U, V>.NonGenericInner' declared here}}
}

struct Y { // expected-note{{extended type 'Y' declared here}}
  struct Inner<A, B : P3> { }

  struct NonGenericInner { } 
}

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

// Name lookup of generic parameters.
extension X<A, B, C> { // expected-error{{generic arguments are not allowed on an extension}}
  // Okay: generic parameters from the extension.
  func foo(x: A) -> (B, C) { }

  // Okay: associated types of the generic parameters 
  func bar(x: A.AssocType) { }

  // Ill-formed: generic parameters from the extended type.
  func wibble(x: T) { } // expected-error{{use of undeclared type 'T'}}
}

// Using generic extensions (basic).
func f1<A, B, C>(x: X<A, B, C>, a: A, assoc: A.AssocType) {
  var (b, c): (B, C) = x.foo(a)
  x.bar(assoc)
}

// Good: Extensions of nested generics.
extension X<T, U, V>.Inner<A, B> { } // FIXME: expected-error{{extension of generic type 'X<T, U, V>.Inner' cannot add requirements}}

// Bad: Extensions of nested generics with wrong number of arguments.
extension X<T, U, V>.Inner<A> { } // expected-error{{extension of generic type 'X<T, U, V>.Inner' has too few generic parameters (have 1, expected 2)}}
extension X<T, U, V>.Inner<A, B, C> { } // expected-error{{extension of generic type 'X<T, U, V>.Inner' has too many generic parameters (have 3, expected 2)}}

// Bad: Extensions with generic parameter lists in the wrong places.
extension X<T, U, V>.NonGenericInner<A> { } // expected-error{{'X<T, U, V>.NonGenericInner' does not have any generic parameters}}
extension Y<T>.Inner<A, B> { } // expected-error{{'Y' does not have any generic parameters}}
// expected-error @-1{{generic arguments are not allowed on an extension}}

