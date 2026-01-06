// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s %S/Inputs/differentiation_diagnostics_other_file.swift -module-name main -o /dev/null

// Test differentiation transform cross-file diagnostics.

import _Differentiation

// TF-1271: Test `@differentiable` original function in other file.
@differentiable(reverse)
func crossFileDifferentiableAttr<T: Protocol>(
  _ input: T
) -> T {
  return input.identityDifferentiableAttr()
}

// TF-1272: Test original function with registered derivatives in other files.
@differentiable(reverse)
func crossFileDerivativeAttr<T: Protocol>(
  _ input: T
) -> T {
  // No error expected
  return input.identityDerivativeAttr()
}

@differentiable(reverse)
func crossFileDerivativeAttr(_ input: Struct) -> Struct {
  // No error expected
  return input.identityDerivativeAttr()
}

// TF-1234: Test `@differentiable` propagation from protocol requirement storage
// declarations to their accessors in other file.
@differentiable(reverse)
func protocolRequirementGetters<T: Protocol>(_ x: T) -> Float {
  // No error expected
  x.property + x[]
}

// TF-1184: Make `@differentiable` on storage declarations propagate to
// the setter in addition to the getter.
@differentiable(reverse)
func protocolRequirementSetters<T: Protocol>(_ x: inout T, _ newValue: Float) {
  // No error expected
  x.property = newValue
  // No error expected
  x[] = newValue
}

// TF-1234: Test `@differentiable` propagation from class member storage
// declarations to their accessors in other file.
@differentiable(reverse)
func classRequirementGetters(_ x: Class) -> Float {
  // No error expected
  x.property + x[]
}

@differentiable(reverse)
func classRequirementSetters(_ x: inout Class, _ newValue: Float) {
  x.property = newValue
  x[] = newValue
}

// Test cross-file lookup of a derivative function with all-concrete derivative generic signature.
@differentiable(reverse)
func allConcreteDerivativeGenericSignature(_ a: [S]) -> Float {
  // No error expected.
  return a.sum()
}
