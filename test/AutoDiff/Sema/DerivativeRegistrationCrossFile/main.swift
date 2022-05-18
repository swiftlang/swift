// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s %S/Inputs/derivatives.swift -module-name main -o /dev/null

import _Differentiation

@differentiable(reverse)
func clamp(_ value: Double, _ lowerBound: Double, _ upperBound: Double) -> Double {
    // No error expected
    return max(min(value, upperBound), lowerBound)
}
