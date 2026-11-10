// RUN: %target-typecheck-verify-swift -enable-experimental-static-assert

#assert(true, 123) // expected-cparser-error{{expected a string literal}} expected-astgen-error {{expected string literal}}

#assert(true, "error \(1) message") // expected-cparser-error{{'#assert' message cannot be an interpolated string literal}} expected-astgen-error {{argument cannot be an interpolated string literal}}

#assert true, "error message") // expected-cparser-error{{expected '(' in #assert directive}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} expected-astgen-warning {{boolean literal is unused}} expected-astgen-error {{unexpected code ', "error message")' in source file}}

#assert(, "error message") // expected-cparser-error{{expected a condition expression}} expected-astgen-error {{expected value in macro expansion}}

func unbalanced1() {
  #assert(true // expected-cparser-error{{expected ')' in #assert directive}} expected-astgen-error {{expected ')' to end macro expansion}}
  // expected-note @-1 {{to match this opening '('}}
}

func unbalanced2() {
  #assert(true, "hello world" // expected-cparser-error{{expected ')' in #assert directive}} expected-astgen-error {{expected ')' to end macro expansion}}
  // expected-note @-1 {{to match this opening '('}}
}
