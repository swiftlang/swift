// RUN: %target-parse-verify-swift -parse-as-library

var _ = 1 // expected-error{{global variable declaration does not bind any variables}}

func foo() {
  var _ = 1 // OK
}

struct Foo {
  var _ = 1 // expected-error{{property declaration does not bind any variables}}
  var (_, _) = (1, 2) // expected-error{{property declaration does not bind any variables}}

  func foo() {
    var _ = 1 // OK
  }
}
