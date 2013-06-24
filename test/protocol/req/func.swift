// RUN: %swift -parse %s -verify

// Test function requirements within protocols, as well as conformance to
// said protocols.

// Simple function
protocol P1 {
  func f0()
}

// Simple match
struct X1a : P1 {
  func f0() {} 
}

// Simple match selecting among two overloads.
struct X1b : P1 {
  func f0() -> Int {}
  func f0() {} 
}

// Function with an associated type
protocol P2 {
  typealias Assoc : P1 // FIXME: expected-note{{protocol requires nested type 'Assoc'}} expected-note{{protocol requires nested type 'Assoc'}} expected-note{{protocol requires nested type 'Assoc'}}
  func f1(x : Assoc) // FIXME: expected-note{{multiple matching functions named 'f1' with type '(Assoc) -> ()'}} expected-note{{protocol requires function 'f1' with type '(Assoc) -> ()'}}
}

// Exact match.
struct X2a : P2 {
  typealias Assoc = X1a

  func f1(x : X1a) {} 
}

// Matches with different function parameter names.
struct X2b : P2 {
  typealias Assoc = X1a

  func f1(y : X1a) {} 
}

struct X2c : P2 {
  typealias Assoc = X1a

  func f1(_ : X1a) {} 
}

// Select among overloads.
struct X2d : P2 {
  typealias Assoc = X1a

  func f1(x : Int) { }
  func f1(x : X1a) { }
}

struct X2e : P2 {
  typealias Assoc = X1a

  func f1(x : X1b) { }
  func f1(x : X1a) { }
}

// Select among overloads distinguished by name.
struct X2f : P2 { // FIXME: expected-error{{type 'X2f' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  func f1(y : X1a) { } // expected-note{{possibly intended match with type '(X1a) -> ()'}}
  func f1(x : X1a) { } // expected-note{{possibly intended match with type '(X1a) -> ()'}}
}

// Infer associated type from function parameter
// FIXME: Also check return type, nested cases
struct X2g : P2 { // FIXME: expected-error{{type 'X2g' does not conform to protocol 'P2'}}
  func f1(x : X1a) { }
}

// Deduction of type that doesn't meet requirements
struct X2x : P2 { // expected-error{{type 'X2x' does not conform to protocol 'P2'}}
  func f1(x : Int) { } 
}

// Mismatch in parameter types
struct X2y : P2 { // expected-error{{type 'X2y' does not conform to protocol 'P2'}}
  typealias Assoc = X1a
  func f1(x : X1b) { } // expected-note{{possibly intended match with type '(X1b) -> ()'}}
}

// Ambiguous deduction
struct X2z : P2 { // expected-error{{type 'X2z' does not conform to protocol 'P2'}}
  func f1(x : X1a) { }
  func f1(x : X1b) { }
}

