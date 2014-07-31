// RUN: %swift -parse -module-name generics %s -verify

protocol P1 { typealias AssocType }
protocol P2 : P1 { }
protocol P3 { }

struct X<T : P1, U : P2, V> { 
  struct Inner<A, B : P3> { } // expected-error{{generic type 'Inner' nested in type}}

  struct NonGenericInner { } // expected-note{{extended type 'X<T, U, V>.NonGenericInner' declared here}} expected-error{{nested in generic type}}
}

struct Y { // expected-note{{extended type 'Y' declared here}}
  struct Inner<A, B : P3> { } // expected-error{{generic type 'Inner' nested in type}}

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

// Good: Extensions of nested generics.
extension X<T, U, V>.Inner<A, B> { } // expected-error{{generic arguments are not allowed on an extension}}
extension X<T : P1, U : P2, V>.Inner<A, B : P3> { } // expected-error{{generic arguments are not allowed on an extension}}
extension X<T, U : P1, V>.Inner<A, B : P3> { } // expected-error{{generic arguments are not allowed on an extension}}
extension generics.X<T, U, V>.Inner<A, B> { } // expected-error{{generic arguments are not allowed on an extension}}

// Bad: Extensions of nested generics with wrong number of arguments.
extension X<T, U, V>.Inner<A> { } // expected-error{{extension of generic type 'X<T, U, V>.Inner' has too few generic parameters (have 1, expected 2)}}
extension X<T, U, V>.Inner<A, B, C> { } // expected-error{{extension of generic type 'X<T, U, V>.Inner' has too many generic parameters (have 3, expected 2)}}

// Bad: Extensions with generic parameter lists in the wrong places.
extension X<T, U, V>.NonGenericInner<A> { } // expected-error{{'X<T, U, V>.NonGenericInner' does not have any generic parameters}}
extension Y<T>.Inner<A, B> { } // expected-error{{'Y' does not have any generic parameters}}
// expected-error @-1{{generic arguments are not allowed on an extension}}
extension generics<T>.X<A, B, C> { } // expected-error{{'generics' does not have any generic parameters}}
// expected-error @-1{{generic arguments are not allowed on an extension}}

// Bad: Extensions of nested generics with extraneous requirements.
extension X<T, U, V: P3>.Inner<A, B> { } // expected-error{{extension of generic type 'X' cannot add requirements}}
extension X<A, B, C>.Inner<A: P1, B> { } // expected-error{{extension of generic type 'X<T, U, V>.Inner' cannot add requirements}}

// Name lookup of generic parameters.
extension X<A, B, C> { // expected-error{{generic arguments are not allowed on an extension}}
  // Okay: generic parameters from the extension.
  func foo(x: A) -> (B, C) { }

  // Okay: associated types of the generic parameters 
  func bar(x: A.AssocType) { }

  // Ill-formed: generic parameters from the extended type.
  func wibble(x: T) { } // expected-error{{use of undeclared type 'T'}}
}

extension X<A, B, C>.Inner<T, U> { // expected-error{{generic arguments are not allowed on an extension}}
  func honk(x: T, y: U) { }
}

// Using generic extensions (basic).
func f1<A, B, C, D, E>(x: X<A, B, C>, a: A, assoc: A.AssocType,
                 inner: X<A, B, C>.Inner<D, E>, d: D, e: E) {
  var (b, c): (B, C) = x.foo(a)
  x.bar(assoc)
  inner.honk(d, y: e)
}

// Lvalue check when the archetypes are not the same.
struct LValueCheck<T> {
  let x = 0
}

extension LValueCheck<A> { // expected-error{{generic arguments are not allowed on an extension}}
  init(newY: Int) {
    x = 42
  }
}
