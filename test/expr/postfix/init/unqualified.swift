// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/43464

class Aaron {
  init(x: Int) { // expected-note {{found this candidate}}
    func foo() {
      // Make sure we recover and assume 'self.init'.
      // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{11-11=self.}}
      // expected-error@+1 {{ambiguous use of 'init'}}
      _ = init
    }
  }
  convenience init() { // expected-note {{found this candidate}}
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{5-5=self.}}
    // expected-error@+1 {{cannot convert value of type 'Bool' to expected argument type 'Int'}}
    init(x: true)

    // FIXME: self.init considered initializer delegation in nested function?
    // expected-error@+2 {{initializer delegation ('self.init') cannot be nested in another expression}}
    // expected-error@+1 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{22-22=self.}}
    func foo() { _ = init() }
  }

  required init(y: Int) {} // expected-note {{found this candidate}}

  static func aaronFn() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{9-9=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'z:', expected 'y:')}}
    _ = init(z: 0)
  }

  // Make sure we recover and assume 'self.init'.
  // expected-error@+3 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{45-45=self.}}
  // expected-error@+2 {{cannot convert value of type 'Aaron' to specified type 'Int'}}
  // expected-error@+1 {{incorrect argument label in call (have 'z:', expected 'y:')}}
  static var aaronVar: Aaron { let _: Int = init(z: 0) }
}

class Theodosia: Aaron {
  init() { // expected-note {{found this candidate}}
    // Make sure we recover and assume 'super.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'super.'?}} {{5-5=super.}}
    // expected-error@+1 {{cannot convert value of type 'Bool' to expected argument type 'Int'}}
    init(x: true)

    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{22-22=self.}}
    // expected-error@+1 {{ambiguous use of 'init'}}
    func foo() { _ = init }
  }

  required init(y: Int) {} // expected-note {{found this candidate}}

  static func theodosiaFn() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{9-9=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'z:', expected 'y:')}}
    _ = init(z: 0)

    // FIXME: We could optimistically parse this as an expression instead
    // expected-error@+2 {{initializers may only be declared within a type}}
    // expected-error@+1 {{expected parameter type following ':'}}
    init(z: 0)
  }

  static var theodosiaVar: Aaron {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{9-9=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'z:', expected 'y:')}}
    _ = init(z: 0)
  }
}

struct AaronStruct {
  init(x: Int) {}
  init() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{5-5=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'y:', expected 'x:')}}
    init(y: 1)

    func foo() {
      // Make sure we recover and assume 'self.init'.
      // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{11-11=self.}}
      // expected-error@+1 {{incorrect argument label in call (have 'y:', expected 'x:')}}
      _ = init(y: 1)
    }
  }

  static func aaronFn() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{9-9=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'y:', expected 'x:')}}
    _ = init(y: 1)

    // FIXME: We could optimistically parse this as an expression instead
    // expected-error@+2 {{initializers may only be declared within a type}}
    // expected-error@+1 {{expected parameter type following ':'}}
    init(y: 1)
  }

  static var aaronVar: Aaron {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{9-9=self.}}
    // expected-error@+1 {{incorrect argument label in call (have 'y:', expected 'x:')}}
    _ = init(y: 1)

    // FIXME: We could optimistically parse this as an expression instead
    // expected-error@+3 {{initializers may only be declared within a type}}
    // expected-error@+2 {{consecutive statements on a line must be separated by ';'}}
    // expected-error@+1 {{non-void function should return a value}}
    return init()
  }
}

enum AaronEnum: Int {
  case A = 1

  init(x: Int) {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{5-5=self.}}
    // expected-error@+1 {{cannot convert value of type 'String' to expected argument type 'Int'}}
    init(rawValue: "")!
  }
}

do {
  func foo() {
    // expected-error@+1 {{initializer expression requires explicit access}} {none}}
    _ = init()
  }
}
