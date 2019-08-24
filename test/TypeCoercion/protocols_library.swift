// RUN: %target-typecheck-verify-swift -parse-as-library

func refCoercion(_: inout CustomStringConvertible) {}

// Make sure we don't add the fix-it for globals in non-script files
var fp_2 = ""

let x = refCoercion(&fp_2) // expected-error{{inout argument could be set to a value with a type other than 'String'; use a value declared as type 'CustomStringConvertible' instead}}

func y() {
  refCoercion(&fp_2) // expected-error{{inout argument could be set to a value with a type other than 'String'; use a value declared as type 'CustomStringConvertible' instead}}
}
