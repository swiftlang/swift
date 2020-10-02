// RUN: %target-typecheck-verify-swift -localization-path %S/Inputs -locale en -enable-parser-lookup

_ = "HI!
// expected-error@-1{{unterminated string literal}}
var self1 = self1 // expected-error {{variable used within its own initial value}}
struct Broken {
  var b : Bool = True // expected-error{{cannot find 'True' in scope}}
}
var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
