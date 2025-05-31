// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules

import Aliases

func f() {
  InvalidCall() // expected-error{{cannot find 'InvalidCall' in scope}}
}

func g() {
  V = 32 // expected-error{{cannot assign to value: 'V' is a get-only property}}
}
