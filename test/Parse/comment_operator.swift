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
func test1() { _ = foo/* */?.description }    // expected-error {{expected ':' after '? ...' in ternary expression}}
func test2() { _ = foo/* */! }                // expected-error {{expected expression after operator}}
func test3() { _ = 1/**/+2 }                  // expected-error {{consecutive statements on a line must be separated by ';'}} expected-error {{ambiguous use of operator '+'}}
func test4() { _ = 1+/**/2 }                  // expected-error {{'+' is not a postfix unary operator}} expected-error {{consecutive statements on a line must be separated by ';'}} expected-warning {{integer literal is unused}}

// Continue to be errors
func test5() { _ = !/* */foo }                // expected-error {{unary operator cannot be separated from its operand}}
func test6() { _ = 1+/* */2 }                 // expected-error {{'+' is not a postfix unary operator}} expected-error {{consecutive statements on a line must be separated by ';'}} expected-warning {{integer literal is unused}}

// Continue to work
_ = foo!// this is dangerous
_ = 1 +/**/ 2
_ = 1 +/* hi */2

// Ensure built-in operators are properly tokenized.
_ =/**/2
_/**/= 2
typealias A = () ->/* */()
func test7(x: Int) { _ = x./* desc */ } // expected-error {{expected member name following '.'}}
