// RUN: %swift -parse %s -verify

// Tests for initializer delegation via self.init(...).

// Initializer delegation: classes
class C0 {
  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
}

class C1 {
  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: structs
struct S0 {
  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
}

struct S1 {
  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Initializer delegation: enum
enum E0 {
  case A
  case B

  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
}

enum E1 {
  case A
  case B

  init() {
    self.init(5)
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initializer delegation: no matching constructor
class Z0 {
  init() {
    self.init(5, 5) // expected-error{{expression does not type-check}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

struct Z1 {
  init() {
    self.init(5, 5) // expected-error{{expression does not type-check}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

enum Z2 {
  case A
  case B

  init() {
    self.init(5, 5) // expected-error{{expression does not type-check}}
  }

  init(value: Int) { /* ... */ }
  init(value: Double) { /* ... */ }
}

// Ill-formed initialization: wrong context.
class Z3 {
  func f() {
    self.init() // expected-error{{initializer delegation can only occur within an initiailizer}}
  }

  init() { }
}

// Ill-formed initializer: refers to an initializer that isn't 'self' or 'super'.
class Z4 {
  init() {}

  init(other: Z4) {
    other.init() // expected-error{{'init' can only refer to the initializers of 'self'}}
  }
}

class Z5 : Z4 {
  init() { }

  init(other: Z5) {
    other.init() // expected-error{{'init' can only refer to the initializers of 'self' or 'super'}}
  }
}
