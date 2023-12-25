// RUN: %target-typecheck-verify-swift

struct FooStructConstructorA {
  init // expected-error {{expected '('}}
  // expected-error@-1{{initializer requires a body}}
}

struct FooStructConstructorB {
  init() // expected-error {{initializer requires a body}}
}

struct FooStructConstructorC {
  init {} // expected-error {{expected '('}}{{7-7=()}}
  // expected-note@-1{{'init()' previously declared here}}
  init<T> {} // expected-error {{expected '('}} {{10-10=()}}
  // expected-error@-1{{generic parameter 'T' is not used in function signature}}
  init? { self.init() } // expected-error {{expected '('}} {{8-8=()}}
  // expected-error@-1{{invalid redeclaration of 'init()'}}
}

struct FooStructConstructorD {
  init() -> FooStructConstructorD { }
  // expected-error@-1{{initializers cannot have a result type}}
}

struct FooStructDeinitializerA {
  deinit // expected-error {{expected '{' for deinitializer}}
  deinit x // expected-error {{deinitializers cannot have a name}} {{10-12=}}  expected-error {{expected '{' for deinitializer}}
  deinit x() // expected-error {{deinitializers cannot have a name}} {{10-11=}} expected-error {{no parameter clause allowed on deinitializer}} {{11-13=}} expected-error {{expected '{' for deinitializer}}
}

struct FooStructDeinitializerB {
  deinit // expected-error {{expected '{' for deinitializer}}
}

struct FooStructDeinitializerC {
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

class FooClassDeinitializerA {
  deinit(a : Int) {} // expected-error{{no parameter clause allowed on deinitializer}}{{9-18=}}
}

class FooClassDeinitializerB {
  deinit { }
}

class FooClassDeinitializerC {
  deinit x (a : Int) {} // expected-error {{deinitializers cannot have a name}} {{10-12=}} expected-error{{no parameter clause allowed on deinitializer}}{{12-22=}}
}

init {} // expected-error {{initializers may only be declared within a type}} expected-error {{expected '('}} {{5-5=()}}
init() // expected-error {{initializers may only be declared within a type}}
init() {} // expected-error {{initializers may only be declared within a type}}

deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
deinit // expected-error {{expected '{' for deinitializer}}
deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}

struct BarStruct {
  init() {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

extension BarStruct {
  init(x : Int) {}

  // When/if we allow 'var' in extensions, then we should also allow dtors
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

enum BarUnion {
  init() {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

extension BarUnion {
  init(x : Int) {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

class BarClass {
  init() {}
  deinit {}
}

extension BarClass {
  convenience init(x : Int) { self.init() }
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

protocol BarProtocol {
  init() {} // expected-error {{protocol initializers must not have bodies}}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

extension BarProtocol {
  init(x : Int) {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

func fooFunc() {
  init() {} // expected-error {{initializers may only be declared within a type}}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
}

func barFunc() {
  var x : () = { () -> () in
    // expected-warning@-1 {{variable 'x' was never used; consider replacing with '_' or removing it}}
    init() {} // expected-error {{initializers may only be declared within a type}}
    return
  } ()

  var y : () = { () -> () in
    // expected-warning@-1 {{variable 'y' was never used; consider replacing with '_' or removing it}}
    deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or noncopyable type}}
    return
  } ()
}

// https://github.com/apple/swift/issues/43464

class Aaron {
  init(x: Int) {
    func foo() {
      // Make sure we recover and assume 'self.init'.
      // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{11-11=self.}}
      // expected-error@+1 {{type of expression is ambiguous without a type annotation}}
      _ = init
    }
  }
  convenience init() {
    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{5-5=self.}}
    // expected-error@+1 {{cannot convert value of type 'Bool' to expected argument type 'Int'}}
    init(x: true)

    // FIXME: self.init considered initializer delegation in nested function?
    // expected-error@+2 {{initializer delegation ('self.init') cannot be nested in another expression}}
    // expected-error@+1 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{22-22=self.}}
    func foo() { _ = init() }
  }

  required init(y: Int) {}

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
  init() {
    // Make sure we recover and assume 'super.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'super.'?}} {{5-5=super.}}
    // expected-error@+1 {{cannot convert value of type 'Bool' to expected argument type 'Int'}}
    init(x: true)

    // Make sure we recover and assume 'self.init'.
    // expected-error@+2 {{initializer expression requires explicit access; did you mean to prepend it with 'self.'?}} {{22-22=self.}}
    // expected-error@+1 {{type of expression is ambiguous without a type annotation}}
    func foo() { _ = init }
  }

  required init(y: Int) {}

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
