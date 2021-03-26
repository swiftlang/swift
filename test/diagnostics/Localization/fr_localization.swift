// RUN: %empty-directory(%t)
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/fr.yaml --output-directory=%t/
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/en.yaml --output-directory=%t/
// RUN: %target-typecheck-verify-swift -localization-path %t -locale fr

_ = "HI!
// expected-error@-1{{chaîne non terminée littérale}}

// FIXME: This used to produce a localized diagnostic.

var self1 = self1 // expected-note 2{{through reference here}}
// expected-error@-1 {{circular reference}}

struct Broken {
  var b : Bool = True // expected-error{{impossible de trouver 'True' portée}}
}
var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
