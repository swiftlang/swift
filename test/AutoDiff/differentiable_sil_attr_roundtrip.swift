// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -o %t/roundtrip.sil
// RUN: %target-swift-frontend -emit-sil %t/roundtrip.sil

// TF-656: Verify that `AutoDiffIndexSubset` for SIL `[differentiable]`
// attribute is set correctly.

// Otherwise, an assertion is triggered during the differentiation transform:
// Assertion failed: (newCapacity >= capacity), function extendingCapacity
// ... ADContext::promoteToDifferentiableFunction

@differentiable(wrt: x)
func TF_656(_ x: Float, _ y: Float) -> Float {
  return x + y
}
_ = gradient(at: 1, in: { x in TF_656(x, 2) })
