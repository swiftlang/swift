// RUN: %target-swift-frontend -parse -verify %s

struct S0 {
  @_implements(1, Foo) // expected-error {{expected type}}
  func f() { }
}

struct S1 {
  @_implements(NeedsF0, 1) // expected-error {{expected a member name as second parameter in '_implements' attribute}}
  func f() { }
}

struct S2 {
  @_implements(NeedsF0) // expected-error {{expected ',' in '_implements' attribute}}
  func f() { }
}
