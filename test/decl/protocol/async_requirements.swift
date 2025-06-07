// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// Ensure that a protocol with async requirements can be conformed to by
// non-async requirements, and that overloading works.
protocol A {
  func foo()
  func foo() async

  init()
  init() async

  var property: Int { get async }

  func bar() throws
  func bar() async throws
}

struct A1: A {
  func foo() { }

  var property: Int = 17

  func bar() { }
}

struct A2: A {
  func foo() { }
  func foo() async { }

  init() { }
  init() async { }

  var property: Int { get async { 5 } }

  func bar() throws { }
  func bar() async throws { }
}
