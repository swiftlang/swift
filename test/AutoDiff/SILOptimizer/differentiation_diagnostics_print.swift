// RUN: %target-swift-frontend -emit-sil -verify -Xllvm --sil-print-after=differentiation -Xllvm --debug-only=differentiation %s 2>&1 | %FileCheck %s

// Needed for '--debug-only'

import _Differentiation

// Helps increase code coverage for DifferentiationInvoker.cpp:47-50
// Copy of differentiation_diagnostics.swift:fragileFuncWithGradient, with different names but same signature
public func mirror(_ x: Float) -> Float { x }

@inlinable
func useInlineFunc() {
  // expected-error @+2 {{function is not differentiable}}
  // expected-note @+1 {{differentiated functions in '@inlinable' functions must be marked '@differentiable' or have a public '@derivative'}}
  _ = gradient(at: 0, of: mirror)
}
