// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

@differentiable(reverse, wrt: x)
public func branchingStep(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// These three VJP calls exercise reuse of the same caller classification.
@differentiable(reverse, wrt: x)
public func repeatedBranchCalls(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  return result
}

// CHECK-LABEL: // reverse-mode derivative of repeatedBranchCalls(_:_:)
// CHECK-NOT:     function_ref @$s3src13branchingStepyS2f_SbtFTJr
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of repeatedBranchCalls(_:_:)
// CHECK-NEXT:    %[[#FN:]] = function_ref @[[PULLBACK:[^ ]+]] : $@convention(thin) (Float, [[BTE:_AD__[^ ]+branchingStep[^ ]+_spec[^, ]+]], [[BTE]], [[BTE]]) -> Float
// CHECK-NEXT:    %[[#PB:]] = partial_apply [callee_guaranteed] %[[#FN]]
// CHECK-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// CHECK-NEXT:    return %[[#RESULT]]
// CHECK-NEXT:  } // end sil function

// CHECK:       sil private @[[PULLBACK]] :
// CHECK-COUNT-3: switch_enum %[[#]], case #[[BTE]].
// CHECK:       } // end sil function

public func repeatedGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { repeatedBranchCalls($0, flag) })
}

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
