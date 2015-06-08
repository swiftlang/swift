// RUN: %target-parse-verify-swift

// Tests for initializer delegation via self.init(...).

// Initializer delegation: classes
class C0 {
  convenience init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

class C1 {
  convenience init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: structs
struct S0 {
  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

struct S1 {
  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: enum
enum E0 {
  case A
  case B

  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
}

enum E1 {
  case A
  case B

  init() {
    self.init(value: 5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initializer delegation: no matching constructor
class Z0 {
  init() { // expected-error {{designated initializer for 'Z0' cannot delegate (with 'self.init'); did you mean this to be a convenience initializer?}}
    self.init(5, 5) // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}} expected-note{{delegation occurs here}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

struct Z1 {
  init() {
    self.init(5, 5) // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

enum Z2 {
  case A
  case B

  init() {
    self.init(5, 5) // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initialization: wrong context.
class Z3 {
  func f() {
    self.init() // expected-error{{initializer delegation can only occur within an initializer}}
  }

  init() { }
}

// Ill-formed initializer: refers to an initializer that isn't 'self' or 'super'.
class Z4 {
  init() {}

  convenience init(other: Z4) {
    other.init() // expected-error{{'init' can only refer to the initializers of 'self'}}
  }
}

class Z5 : Z4 {
  override init() { }

  convenience init(other: Z5) {
    other.init() // expected-error{{'init' can only refer to the initializers of 'self' or 'super'}}
  }
}

// Ill-formed initialization: failure to call initializer.
class Z6 {
  convenience init() {
    var _ : () -> Z6 = self.init // expected-error{{partial application of 'self.init' initializer delegation is not allowed}}
  }

  init(other: Z6) { }
}

// Ill-formed initialization: both superclass and delegating.
class Z7Base { }

class Z7 : Z7Base {
  override init() { }

  init(b: Bool) {
    if b { super.init() } // expected-note{{previous chaining call is here}}
    else { self.init() } // expected-error{{initializer cannot both delegate ('self.init') and chain to a }}
  }
}

struct RDar16603812 {
   var i = 42
   init() {}
   func foo() {
      self.init() // expected-error {{initializer delegation can only occur within an initializer}}
   }
}

class RDar16666631 {
   var i: Int
   var d: Double
   var s: String
   init(i: Int, d: Double, s: String) {
      self.i = i
      self.d = d
      self.s = s
   }
   convenience init(i: Int, s: String) {
      self.init(i: i, d: 0.1, s: s)
   }
}
let rdar16666631 = RDar16666631(i: 5, d: 6) // expected-error {{missing argument for parameter 's' in call}}

