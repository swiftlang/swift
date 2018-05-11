// RUN: %target-typecheck-verify-swift

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
  init() { } // expected-error{{initializer requirement 'init()' can only be satisfied by a 'required' initializer in non-final class 'C1c'}}{{3-3=required }}
}

struct S2 : P1 { } // okay

enum E2 : P1 { } // expected-error{{type 'E2' does not conform to protocol 'P1'}}

final class C2a : P1 { } // okay

class C2b : P1 { } // expected-error{{initializer requirement 'init()' can only be satisfied by a 'required' initializer in non-final class 'C2b'}}

class C2c { 
  init(x: Int) { }
}

extension C2c : P1 {
  convenience init() { self.init(x: 0) } // expected-error{{initializer requirement 'init()' can only be satisfied by a 'required' initializer in the definition of non-final class 'C2c'}}
}

class C2d { 
  init(x: Int) { }
  convenience init() { self.init(x: 0) } // expected-note {{'init()' declared here}} {{3-3=required }}
}

extension C2d : P1 { // expected-error{{initializer requirement 'init()' can only be satisfied by a 'required' initializer in non-final class 'C2d'}}
}


// rdar://problem/24575507
protocol P2 {
  init()
  init(int: Int)
}

extension P2 {
  init() {
    self.init(int: 17)
  }
}


class Foo : P2 {
  var value: Int

  // okay: init() requirement satisfied by protocol extension

  required init(int value: Int) {
    self.value = value
  }
}
