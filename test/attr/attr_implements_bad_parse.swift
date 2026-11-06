// RUN: %target-swift-frontend -parse -verify %s

struct S0 {
  @_implements(1, Foo) // expected-error {{expected type}} expected-astgen-error {{unexpected code '1' before @_implements arguments}}
  func f() { }
}

struct S1 {
  @_implements(NeedsF0, 1) // expected-cparser-error {{expected a member name as second parameter in '_implements' attribute}} expected-astgen-error {{expected declaration name in @_implements arguments}} expected-astgen-error {{unexpected code '1' in attribute}}
  func f() { }
}

struct S2 {
  @_implements(NeedsF0) // expected-cparser-error {{expected ',' in '_implements' attribute}} expected-astgen-error {{expected ',' and declaration name in @_implements arguments}}
  func f() { }
}
