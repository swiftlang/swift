// RUN: %target-swift-frontend -typecheck %s -verify

///////          ///////
///////  ERRORS  ///////
///////          ///////

// MARK: Missing 'super' call

class Invalid1A {
  @requiresSuper
  func foo(bar: Int) {}
}

class Invalid1B: Invalid1A {
  override func foo(bar: Int) {}
  // expected-error@-1 {{method override is missing 'super.foo(bar:)' call}}
}

// MARK: Incorrect 'super' call

class Invalid2A {
  @requiresSuper
  func foo(bar: Int) {}

  func foo(baz: Int) {}
}

class Invalid2B: Invalid2A {
  override func foo(bar: Int) { 
    // expected-error@-1 {{method override is missing 'super.foo(bar:)' call}}
    super.foo(baz: bar)
  }
}

// MARK: Cannot require 'super' call on 'final' method

final class Invalid3A {
  @requiresSuper
  // expected-error@-1 {{'@requiresSuper' cannot be applied to final method}} {{3-17=}}
  func foo(bar: Int) {}
}

// MARK: Inherits '@requiresSuper'
class BaseRequiresSuper {
  @requiresSuper
  func foo() { }
}

class OverrideWhichCallsSuperInBody: BaseRequiresSuper {
  override func foo() { super.foo() }
}

class OverrideWhichSkipsSuperInBody: OverrideWhichCallsSuperInBody {
  override func foo() { } 
  // expected-error@-1 {{method override is missing 'super.foo()' call}}
}

struct Invalid4A {
  @requiresSuper
  // expected-error@-1 {{'@requiresSuper' can only be applied on non-final class methods}}
  func foo() {}
}

class Invalid5A {
  @requiresSuper 
  // expected-error@-1 {{'@requiresSuper' attribute cannot be applied to this declaration}}
  static var foo1: Int = 0

  @requiresSuper 
  // expected-error@-1 {{'@requiresSuper' attribute cannot be applied to this declaration}}
  final var foo2: Int = 1

  @requiresSuper 
  // expected-error@-1 {{'@requiresSuper' attribute cannot be applied to this declaration}}
  var foo3: Int = 2

  @requiresSuper 
  // expected-error@-1 {{'@requiresSuper' attribute cannot be applied to this declaration}}
  subscript(i: Int) -> Int { fatalError() }
}

class Invalid6A { // expected-note {{in declaration of 'Invalid6A'}}
  @requiresSuper("")
  // expected-error@-1 {{@requiresSuper message must not be empty}}
  // expected-error@-2 {{expected declaration}}
  func foo1() {}
}

class Invalid7A {
  @requiresSuper
  init() {}
}

class Invalid7B: Invalid7A {
  override init() {}
  // expected-error@-1 {{method override is missing 'super.init()' call}}
}

///////              ///////
///////  VALID USES  ///////
///////              ///////

// MARK: Valid 'super' call
class Valid1A {
  @requiresSuper
  func foo(bar: Int) {}
}

class Valid1B: Valid1A {
  override func foo(bar: Int) { // Okay
    super.foo(bar: bar)
  }
}

class Valid2A {
  @requiresSuper
  init() {}
}

class Valid2B: Valid2A {
  override init() { super.init() } // Okay
}

// MARK: Shows optional message on attribute
class Valid3A {
  @requiresSuper("call super as final step in your implementation")
  func foo(bar: Int) {}
}

class Valid3B: Valid3A {
  override func foo(bar: Int) {}
  // expected-error@-1 {{method override is missing 'super.foo(bar:)' call: call super as final step in your implementation}}
}

// MARK: Valid behavior of inherited attributes
class OverrideWhichCallsSuperInBody2: OverrideWhichCallsSuperInBody {
  override func foo() { super.foo() } // Okay
}
