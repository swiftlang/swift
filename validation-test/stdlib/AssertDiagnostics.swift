// RUN: %swift -verify -parse %s

func rejectsStringLiteral() {
  assert("foo") // expected-error {{cannot convert the expression's type}}
  precondition("foo") // expected-error {{cannot convert the expression's type}}
}

