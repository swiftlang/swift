// RUN: %target-typecheck-verify-swift

class A {
  func foo() -> A { return self }
}

class B: A {
  @_nonoverride
  func foo() -> B { return self }

  @_nonoverride override // expected-error{{'override' cannot be combined with '@_nonoverride'}}
  func foo() -> Int { return 0 }
}

struct X {
  @_nonoverride func foo() { } // expected-error{{'@_nonoverride' can only be specified on class or protocol members}}
}
