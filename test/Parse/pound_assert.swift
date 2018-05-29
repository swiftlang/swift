// RUN: %target-typecheck-verify-swift

#assert(true, 123) // expected-error{{expected a string literal}}

#assert(true, "error \(1) message") // expected-error{{interpolated strings not allowed as #assert messages}}

#assert true, "error message") // expected-error{{expected '(' in #assert directive}}

#assert(, "error message") // expected-error{{expected a condition expression}}

func unbalanced1() {
  #assert(true // expected-error{{expected ')' in #assert directive}}
}

func unbalanced2() {
  #assert(true, "hello world" // expected-error{{expected ')' in #assert directive}}
}
