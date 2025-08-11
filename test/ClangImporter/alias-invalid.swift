// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -I %S/Inputs/custom-modules -enable-experimental-feature ImportMacroAliases

// REQUIRES: swift_feature_ImportMacroAliases

import Aliases

func f() {
  InvalidCall() // expected-error{{cannot find 'InvalidCall' in scope}}
}

func g() {
  V = 32 // expected-error{{cannot assign to value: 'V' is a get-only property}}
}

func h() {
  let _ = overload // expected-error{{ambiguous use of 'overload'}}
}

func i() {
  aliased_variadic(0, 0) // expected-error{{cannot find 'aliased_variadic' in scope}}
}
