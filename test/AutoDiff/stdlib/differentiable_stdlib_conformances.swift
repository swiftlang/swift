// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// rdar://82240971 – Temporarily disable this test on non-macOS platforms until
// the CI is fixed to not produce 'Invalid device: iPhone 8'.
// REQUIRES: OS=macosx

import _Differentiation

// Test `Differentiable` protocol conformances for stdlib types.

func assertConformsToDifferentiable<T>(_: T.Type) where T: Differentiable {}

func assertSelfEqualsTangentVector<T>(_: T.Type)
where T: Differentiable, T == T.TangentVector {}

// Test `FloatingPoint` types.
func testFloatingPointDifferentiableConformance() {
  assertSelfEqualsTangentVector(Float.self)
  assertSelfEqualsTangentVector(Double.self)
  #if (arch(i386) || arch(x86_64)) && !(os(Windows) || os(Android))
  assertSelfEqualsTangentVector(Float80.self)
  #endif
}
