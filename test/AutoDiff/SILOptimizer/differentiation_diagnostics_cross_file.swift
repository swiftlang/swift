// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s %S/Inputs/differentiation_diagnostics_other_file.swift -module-name main -o /dev/null

// Test differentiation transform cross-file diagnostics.

import _Differentiation

// TF-1271: Test `@differentiable` original function in other file.
@differentiable
func crossFileDifferentiableAttr<T: Protocol>(
  _ input: T
) -> T {
  return input.identityDifferentiableAttr()
}

// TF-1272: Test original function with registered derivatives in other files.
// FIXME(TF-1272): Find a way to type-check `@derivative` attributes in other
// files.
@differentiable
func crossFileDerivativeAttr<T: Protocol>(
  _ input: T
) -> T {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{cannot differentiate functions that have not been marked '@differentiable' and that are defined in other files}}
  return input.identityDerivativeAttr()
}
