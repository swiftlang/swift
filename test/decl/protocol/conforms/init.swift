// RUN: %swift -parse %s -verify

protocol P1 {
  init() // expected-note{{protocol requires initializer 'init()' with type '()'}}
}

// ------------------------------------------------------------------------
// Conformance to initializer requirements
// ------------------------------------------------------------------------
struct S1 : P1 {
  init() { } // okay
}

enum E1 : P1 {
  case A, B
  
  init() { self = .A } // okay
}

class C1 : P1 {
  init() { } // okay
}

struct S2 : P1 { } // okay

enum E2 : P1 { } // expected-error{{type 'E2' does not conform to protocol 'P1'}}

class C2 : P1 { } // okay


