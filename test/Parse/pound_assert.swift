// RUN: %target-typecheck-verify-swift -enable-experimental-static-assert

#assert(true, 123) // expected-error{{expected a string literal}}

#assert(true, "error \(1) message") // expected-error{{'#assert' message cannot be an interpolated string literal}}

#assert true, "error message") // expected-error{{expected '(' in #assert directive}}

#assert(, "error message") // expected-error{{expected a condition expression}}

func unbalanced1() {
  #assert(true // expected-error{{expected ')' in #assert directive}}
  // expected-note @-1 {{to match this opening '('}}
}

func unbalanced2() {
  #assert(true, "hello world" // expected-error{{expected ')' in #assert directive}}
  // expected-note @-1 {{to match this opening '('}}
}
