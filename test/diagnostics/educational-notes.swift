// RUN: %target-typecheck-verify-swift -enable-descriptive-diagnostics -diagnostic-documentation-path %S/test-docs/

// A diagnostic with no educational notes
let x = 1 +
let y = 2
// expected-error@-1 {{expected expression after operator}} {{educational-notes=none}}

// A diagnostic with an educational note
extension (Int, Int) {}
// expected-error@-1 {{non-nominal type '(Int, Int)' cannot be extended}} {{educational-notes=nominal-types}}

let closureWithoutContext = {
  // expected-error@-1 {{unable to infer complex closure return type}} {{educational-notes=complex-closure-inference}}
  if true {
    return 1
  } else {
    return 2
  }
}
