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

// TF-1234: Test `@differentiable` propagation from protocol requirement storage
// declarations to their accessors in other file.

@differentiable
func protocolRequirementGetters<T: Protocol>(_ x: T) -> Float {
  x.property + x[]
}

// TODO(TF-1184): Make `@differentiable` on storage declarations propagate to
// the setter in addition to the getter.
@differentiable
func protocolRequirementSetters<T: Protocol>(_ x: inout T, _ newValue: Float) {
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
  x.property = newValue
  // expected-error @+2 {{expression is not differentiable}}
  // expected-note @+1 {{member is not differentiable because the corresponding protocol requirement is not '@differentiable'}}
  x[] = newValue
}

// TF-1234: Test `@differentiable` propagation from class member storage
// declarations to their accessors in other file.

@differentiable
func classRequirementGetters(_ x: Class) -> Float {
  x.property + x[]
}

@differentiable
func classRequirementSetters(_ x: inout Class, _ newValue: Float) {
  x.property = newValue
  x[] = newValue
}

// Test cross-file lookup of a derivative function with all-concrete derivative generic signature.
@differentiable
func allConcreteDerivativeGenericSignature(_ a: [S]) -> Float {
  // No error expected.
  return a.sum()
}
