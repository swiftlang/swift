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
  // expected-warning@-1 {{method override is missing 'super.foo(bar:)' call}}
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
    // expected-warning@-1 {{method override is missing 'super.foo(bar:)' call}}
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