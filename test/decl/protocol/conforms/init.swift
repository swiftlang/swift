// RUN: %target-parse-verify-swift

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

class C1a : P1 {
  required init() { } // okay
}

final class C1b : P1 {
  required init() { } // okay
}

class C1c : P1 {
  init() { } // expected-error{{initializer requirement 'init()' can only be satisfied by a `required` initializer in non-final class 'C1c'}}{{3-3=required }}
}

struct S2 : P1 { } // okay

enum E2 : P1 { } // expected-error{{type 'E2' does not conform to protocol 'P1'}}

final class C2a : P1 { } // okay

class C2b : P1 { } // expected-error{{initializer requirement 'init()' can only be satisfied by a `required` initializer in non-final class 'C2b'}}

class C2c { 
  init(x: Int) { }
}

extension C2c : P1 {
  convenience init() { self.init(x: 0) } // expected-error{{initializer requirement 'init()' can only be satisfied by a `required` initializer in the definition of non-final class 'C2c'}}
}

