// RUN: %target-swift-frontend -typecheck -verify -module-name main %s

// Tests for interaction between comments & operators from SE-0037
// which defined comments to be whitespace for operator arity rules.

let foo: Bool! = true

// Used to be errors, should now work
let a = /* */!foo
_ = 1/**/+ 2
_ = 1 /**/+ 2
_ = 1 +/*hi*/2

// Used to work, should now be errors
// The actual error produced is probably not important.
// These are wrapped in functions to allow the parser to recover before the next test case.
func test1() { _ = foo/* */?.description }    // expected-cparser-error {{expected ':' after '? ...' in ternary expression}} expected-astgen-error {{expected ':' and expression after '? ...' in ternary expression}}
func test2() { _ = foo/* */! }                // expected-error {{expected expression after operator}} expected-astgen-error {{'!' is not a binary operator}}
func test3() { _ = 1/**/+2 }                  // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} expected-warning {{result of operator '+' is unused}}
func test4() { _ = 1+/**/2 }                  // expected-error {{'+' is not a postfix unary operator}} expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} expected-warning {{integer literal is unused}}

// Continue to be errors
func test5() { _ = !/* */foo }                // expected-cparser-error {{unary operator cannot be separated from its operand}} expected-astgen-error {{expected expression in function}} expected-astgen-error {{unexpected code '!/* */foo' in function}}
func test6() { _ = 1+/* */2 }                 // expected-error {{'+' is not a postfix unary operator}} expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}} expected-warning {{integer literal is unused}}

// Continue to work
_ = foo!// this is dangerous
_ = 1 +/**/ 2
_ = 1 +/* hi */2

// Ensure built-in operators are properly tokenized.
_ =/**/2
_/**/= 2
typealias A = () ->/* */()
func test7(x: Int) { _ = x./* desc */ } // expected-cparser-error {{expected member name following '.'}} expected-astgen-error {{expected name in member access}}
