// A high call benefit makes runtime-loop VJPs profitable to inline. The
// branching callee must still specialize its tape before callers inline it.
// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src -Xllvm -sil-inline-removed-call-benefit=100 %s | %FileCheck %s

import _Differentiation

public var iterationCount = 3

@differentiable(reverse)
@inline(never)
public func square(_ x: Float) -> Float { x * x }

@differentiable(reverse)
public func loop(_ x: Float) -> Float {
  var result = x
  for _ in 0..<iterationCount {
    result = square(result)
  }
  return result
}

// MARK: - Specialize the branching callee's tape

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branchingLoop(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil @$s3src13branchingLoopyS2f_SbtFTJrSUpSr :
// CHECK:             // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:        %[[#BRANCH_FN:]] = function_ref @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_eaabfde32fa16863f8cd53dbbe0c0f36OD_qyGFAehifoeTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#BRANCH_FN]](

// CHECK-LABEL:     {{^}}// specialized pullback of branchingLoop(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_eaabfde32fa16863f8cd53dbbe0c0f36OD_qyGFAehifoeTf1nb_n :
// CHECK-NOT:         = partial_apply
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func branchingLoop(_ x: Float, _ flag: Bool) -> Float {
  if flag { return loop(x) }
  return x * x
}

// MARK: - Inline the branching VJP after the tape rewrite

// CHECK-LABEL:     {{^}}// reverse-mode derivative of throughBranchingLoop(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil @$s3src20throughBranchingLoopyS2f_SbtFTJrSUpSr :
// CHECK-NOT:         function_ref @$s3src13branchingLoopyS2f_SbtFTJrSUpSr
// CHECK:             // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:        %[[#INLINED_BRANCH:]] = function_ref @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_eaabfde32fa16863f8cd53dbbe0c0f36OD_qyGFAehifoeTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#INLINED_BRANCH]](
// CHECK-NOT:         function_ref @$s3src13branchingLoopyS2f_SbtFTJrSUpSr
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func throughBranchingLoop(_ x: Float, _ flag: Bool) -> Float {
  branchingLoop(x, flag)
}

@differentiable(reverse)
public func scaleLoop(_ x: Float) -> Float { 2 * loop(x) }

@differentiable(reverse)
public func halvingLoop(_ x: Float) -> Float {
  var value = x
  var remaining = iterationCount
  while remaining > 0 {
    value *= 0.5
    remaining -= 1
  }
  return value
}

@differentiable(reverse)
public func forwardHalvingLoop(_ x: inout Float) { x = halvingLoop(x) }

@differentiable(reverse)
public func throughHalvingLoop(_ x: Float) -> Float {
  var value = x
  forwardHalvingLoop(&value)
  return value
}

// MARK: - Use the specialized tape inside a runtime loop

// CHECK-LABEL:     {{^}}// reverse-mode derivative of loopOverBranch(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil @$s3src14loopOverBranchyS2f_SbtFTJrSUpSr :
// CHECK:             %[[#LOOP_CONTEXT:]] = builtin "autoDiffCreateLinearMapContextWithType"
// CHECK:             // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:        %[[#LOOP_BRANCH:]] = function_ref @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_eaabfde32fa16863f8cd53dbbe0c0f36OD_qyGFAehifoeTf1nb_n :
// CHECK:             = partial_apply [callee_guaranteed] %[[#LOOP_BRANCH]](
// CHECK:             = builtin "autoDiffProjectTopLevelSubcontext"(%[[#LOOP_CONTEXT]])
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func loopOverBranch(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  for _ in 0..<iterationCount {
    result = branchingLoop(result, flag)
  }
  return result
}
