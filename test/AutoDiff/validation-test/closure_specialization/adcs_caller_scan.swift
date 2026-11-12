// REQUIRES: executable_test
// RUN: %target-run-simple-swift

import _Differentiation

@differentiable(reverse, wrt: x)
public func branchingStep(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// Each call has a separate branch tape, but all inlining decisions in one
// collection inspect the same caller body. Its classification can be reused.
@differentiable(reverse, wrt: x)
public func repeatedBranchCalls(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  return result
}

public func repeatedGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { repeatedBranchCalls($0, flag) })
}

#if !DEFINITIONS_ONLY
for flag in [true, false] {
  let result = valueWithPullback(at: Float(2), of: {
    repeatedBranchCalls($0, flag)
  })
  let expectedDerivative: Float = flag ? 1024 : 8
  precondition(result.value == (flag ? 256 : 16))
  precondition(repeatedGradient(2, flag) == expectedDerivative)
  for seed in [Float(1), 3, -2] {
    precondition(result.pullback(seed) == expectedDerivative * seed)
  }
}
#endif
