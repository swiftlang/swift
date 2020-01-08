// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -o %t/roundtrip.sil
// RUN: %target-swift-frontend -emit-sil %t/roundtrip.sil

// TF-656: Verify that `AutoDiffIndexSubset` for SIL `[differentiable]`
// attribute is set correctly.

// Otherwise, an assertion is triggered during the differentiation transform:
// Assertion failed: (newCapacity >= capacity), function extendingCapacity
// ... ADContext::promoteToDifferentiableFunction

// NOTE: We cannot differentiate external functions in roundtrip SIL tests.
// Reason: When we print then parse the SIL we lose the information that the
// external function is associated with an AST decl. So the differentiation
// pass can't see the AST differentiable attrs, and the differentiation pass
// thinks that we're trying to differentiate an external function without
// explicit AST differentiable attrs.
// TODO(TF-988): This can probably be fixed.

@differentiable(wrt: x)
func TF_656(_ x: Float, _ y: Float) -> Float {
  // FIXME(TF-988): Cannot differentiate external functions.
  // return x + y
  return 0
}
_ = gradient(at: 1, in: { x in TF_656(x, 2) })
