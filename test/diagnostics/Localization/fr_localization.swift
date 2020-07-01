// RUN: %target-typecheck-verify-swift -localization-path %S/Inputs -locale fr

_ = "HI!
// expected-error@-1{{chaîne non terminée littérale}}
var self1 = self1 // expected-error {{variable utilisée dans sa propre valeur initiale}}
struct Broken {
  var b : Bool = True // expected-error{{impossible de trouver 'True' portée}}
}
var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
