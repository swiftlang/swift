// RUN: %target-typecheck-verify-swift

struct X { }

// Simple default definition for associated types.
protocol P1 {
  associatedtype AssocType1 = Int
}

extension X : P1 { }

var i: X.AssocType1 = 17

// Dependent default definition for associated types
protocol P2 {
  associatedtype AssocType2 = Self
}

extension X : P2 { }
var xAssoc2: X.AssocType2 = X()

// Dependent default definition for associated types that doesn't meet
// requirements.
protocol P3 {
  associatedtype AssocType3 : P1 = Self // expected-note{{default type 'X2' for associated type 'AssocType3' (from protocol 'P3') does not conform to 'P1'}}
}

extension X : P3 { } // okay

struct X2 : P3 { } // expected-error{{type 'X2' does not conform to protocol 'P3'}}
