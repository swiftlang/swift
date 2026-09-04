// RUN: %target-typecheck-verify-swift

func localComputedPropertyMissingType() {
  var int {} // expected-error {{computed property must have an explicit type}} {{10-10=: <# Type #>}} expected-error {{type annotation missing in pattern}}
}

var x { // expected-error {{computed property must have an explicit type}} {{6-6=: <# Type #>}} expected-error {{type annotation missing in pattern}}
  _read {}
}
