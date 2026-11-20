// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name CallerScan -Xllvm -sil-print-before=autodiff-closure-specialization -Xllvm '-sil-print-function=$s10CallerScan19repeatedBranchCallsyS2f_SbtFTJrSUpSr' %s -o %t.sil 2> %t.log
// RUN: %FileCheck %s < %t.log

// CHECK:         *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// CHECK-NEXT:    // reverse-mode derivative of repeatedBranchCalls(_:_:)
// CHECK:           // function_ref reverse-mode derivative of branchingStep(_:_:)
// CHECK-NEXT:      %[[#STEP:]] = function_ref
// CHECK-COUNT-3:   = apply %[[#STEP]](
// CHECK-NOT:       = apply %[[#STEP]](
// CHECK:         } // end sil function

import _Differentiation

@differentiable(reverse, wrt: x)
public func branchingStep(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// Each call needs its own branch tape. The inliner classifies their caller once.
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
