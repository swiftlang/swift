// RUN: %target-typecheck-verify-swift -localization-path %S/Inputs -locale en -disable-experimental-feature ParserASTGen
// These tests use lexer/parser diagnostics, which ASTGen does not localize.

_ = "HI!
// expected-error@-1{{unterminated string literal}}

struct Circular { let x = x }
// expected-note@-1 {{through reference here}}
// expected-error@-2 {{circular reference}}

struct Broken {
  var b : Bool = True // expected-error{{cannot find 'True' in scope}}
}
var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
