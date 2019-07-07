// RUN: %target-typecheck-verify-swift

func variadic1(x: Int...) {}
func variadic2(y: Int..., z: String) {}

func parseErrors1() {
  variadic2(y: #variadic([1,2,3], z: "hello") // expected-error {{expected ')' to complete '#variadic' expression}}
  // expected-note@-1 {{to match this opening '('}}
}

func parseErrors2() {
  variadic1(x: #variadic() ) // expected-error {{expected expression within '#variadic(...)'}}
}

func parseErrors3() {
  variadic1(x: #variadic[1,2,3])) // expected-error {{expected '(' following '#variadic'}}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}
}

func parseErrors4() {
  variadic2(y: #variadic(]), z: "hello") // expected-error {{expected expression within '#variadic(...)'}}
}

