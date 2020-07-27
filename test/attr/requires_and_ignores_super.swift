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
  // expected-note@-2 {{annotate method with '@ignoresSuper' if this is intentional}} {{3-3=@ignoresSuper }}
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
    // expected-note@-2 {{annotate method with '@ignoresSuper' if this is intentional}}
    super.foo(baz: bar)
  }
}

// MARK: Cannot require 'super' call on 'final' method

final class Invalid3A {
  @requiresSuper
  // expected-error@-1 {{'@requiresSuper' cannot be applied to final method}} {{3-17=}}
  func foo(bar: Int) {}
}

class Invalid4A {
  func foo(bar: Int) {}
}

class Invalid4B: Invalid4A {
  @ignoresSuper
  // expected-error@-1 {{'@ignoresSuper' has no effect because base method does not require 'super' call}} {{3-16=}}
  override func foo(bar: Int) {}
}

// MARK: Inherits '@requiresSuper'
class BaseRequiresSuper {
  @requiresSuper
  func foo() { }
}

class OverrideWhichCallsSuperInBody: BaseRequiresSuper {
  override func foo() { super.foo() }
}

class OverrideWhichSkipsSuperInBody1: OverrideWhichCallsSuperInBody {
  override func foo() { } 
  // expected-error@-1 {{method override is missing 'super.foo()' call}}
  // expected-note @-2 {{annotate method with '@ignoresSuper' if this is intentional}}
}

class OverrideWhichHasBothRequiresAndIgnoresSuperAttrs: BaseRequiresSuper {
  @ignoresSuper
  @requiresSuper
  override func foo() { }
}

class OverrideWhichSkipsSuperInBody2: OverrideWhichHasBothRequiresAndIgnoresSuperAttrs {
  override func foo() { }
  // expected-error@-1 {{method override is missing 'super.foo()' call}}
  // expected-note @-2 {{annotate method with '@ignoresSuper' if this is intentional}}
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

// MARK: 'super' call can be skipped
class Valid2A {
  @requiresSuper
  func foo(bar: Int) {}
}

class Valid2B: Valid2A {
  @ignoresSuper
  override func foo(bar: Int) {}
}

// MARK: Shows optional message on attribute
class Valid3A {
  @requiresSuper("call super as final step in your implementation")
  func foo(bar: Int) {}
}

class Valid3B: Valid3A {
  override func foo(bar: Int) {}
  // expected-error@-1 {{method override is missing 'super.foo(bar:)' call: call super as final step in your implementation}}
  // expected-note@-2 {{annotate method with '@ignoresSuper' if this is intentional}}
}

// MARK: Valid behavior of inherited attributes
class OverrideWhichHasIgnoresSuperAttr: BaseRequiresSuper {
  @ignoresSuper
  override func foo() { }
}

class OverrideWhichSkipsSuperInBody3: OverrideWhichHasIgnoresSuperAttr {
  override func foo() {} // Okay, because this doesn't inherit '@requiresSuper'
}
