// RUN: %empty-directory(%t)
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/fr.yaml --output-directory=%t/
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/en.yaml --output-directory=%t/ 2>&1 | %FileCheck %s
// RUN: %target-typecheck-verify-swift -localization-path %t -locale fr -disable-parser-lookup

// CHECK: These diagnostic IDs are no longer availiable: 'not_available_in_def, not_available_in_def_2, not_available_in_def_3, not_available_in_def_4, not_available_in_def_5'
_ = "HI!
// expected-error@-1{{chaîne non terminée littérale}}

// FIXME: This used to produce a localized diagnostic.

var self1 = self1 // expected-note 2{{through reference here}}
// expected-error@-1 {{circular reference}}

struct Broken {
  var b : Bool = True // expected-error{{impossible de trouver 'True' portée}}
}
var v1 : Int[1 // expected-error {{expected ']' in array type}} expected-note {{to match this opening '['}}
