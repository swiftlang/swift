// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s %S/Inputs/derivatives-error.swift -module-name main -o /dev/null

import _Differentiation

@differentiable(reverse)
func clamp(_ value: Double, _ lowerBound: Double, _ upperBound: Double) -> Double {
    return Struct.max(min(value, upperBound), lowerBound) // expected-error {{cannot find 'Struct' in scope}}
}
