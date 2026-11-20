// REQUIRES: asserts

// Raise the call benefit so the loop VJP would be profitable to inline
// before ADCS if the AutoDiff inlining policy allowed it.
// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name loop_vjp_inlining -Xllvm -sil-inline-removed-call-benefit=100 -Xllvm -sil-print-around=autodiff-closure-specialization -Xllvm '-sil-print-function=$s17loop_vjp_inlining13branchingLoopyS2f_SbtFTJrSUpSr,$s17loop_vjp_inlining20throughBranchingLoopyS2f_SbtFTJrSUpSr,$s17loop_vjp_inlining9scaleLoopyS2fFTJrSpSr,$s17loop_vjp_inlining0A10OverBranchyS2f_SbtFTJrSUpSr' %s -o %t.sil 2> %t.log
// RUN: %FileCheck %s --check-prefix=PHASE < %t.log
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil
// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name loop_vjp_inlining %s -o %t.default.sil
// RUN: %FileCheck %s --check-prefix=FORWARD < %t.default.sil

// PHASE:      *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of branchingLoop(_:_:)
// PHASE-NOT:  autoDiffProjectTopLevelSubcontext
// PHASE:        // function_ref reverse-mode derivative of loop(_:)
// PHASE:        apply
// PHASE-NOT:  autoDiffProjectTopLevelSubcontext
// PHASE:      } // end sil function

// PHASE:      *** SIL function after {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of branchingLoop(_:_:)
// PHASE-NOT:  autoDiffProjectTopLevelSubcontext
// PHASE:        // function_ref specialized pullback of branchingLoop(_:_:)
// PHASE-NOT:  autoDiffProjectTopLevelSubcontext
// PHASE:      } // end sil function

// PHASE:      *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of throughBranchingLoop(_:_:)
// PHASE-NOT:  } // end sil function
// PHASE:        // function_ref reverse-mode derivative of branchingLoop(_:_:)
// PHASE-NEXT:   %[[#BRANCH_VJP:]] = function_ref
// PHASE-NEXT:   %[[#]] = apply %[[#BRANCH_VJP]]
// PHASE:      } // end sil function

// An untaped caller can inline a runtime-loop VJP when it captures that
// callee's pullback directly.
// PHASE:      *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of scaleLoop(_:)
// PHASE-NOT:    function_ref reverse-mode derivative of loop
// PHASE:        builtin "autoDiffProjectTopLevelSubcontext"
// PHASE-NOT:    function_ref reverse-mode derivative of loop
// PHASE:      } // end sil function
// PHASE:      *** SIL function after {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of scaleLoop(_:)
// PHASE-NOT:  } // end sil function
// PHASE:        // function_ref specialized pullback of scaleLoop(_:)
// PHASE-NEXT: {{.*}} : $@convention(thin) (Float, @owned Builtin.NativeObject, Float, Float) -> Float
// PHASE:      } // end sil function

// A runtime-loop caller must leave the branching callee eligible for ADCS.
// PHASE:      *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// PHASE-NEXT: // reverse-mode derivative of loopOverBranch(_:_:)
// PHASE-NOT:  } // end sil function
// PHASE:        // function_ref reverse-mode derivative of branchingLoop(_:_:)
// PHASE-NEXT:   %[[#INNER_VJP:]] = function_ref
// PHASE:      {{.*}} = apply %[[#INNER_VJP]]
// PHASE-NOT:  } // end sil function
// PHASE:        builtin "autoDiffProjectTopLevelSubcontext"
// PHASE:      } // end sil function

// FINAL-LABEL: {{^}}// specialized pullback of branchingLoop(_:_:)
// FINAL-NOT:     = partial_apply
// FINAL:       } // end sil function

// Keep the forwarding VJP small enough to inline into its consumer,
// which can then capture the loop tape.
// FORWARD-LABEL: {{^}}// reverse-mode derivative of throughHalvingLoop(_:)
// FORWARD-NOT:     function_ref reverse-mode derivative of forwardHalvingLoop
// FORWARD-NOT:   } // end sil function
// FORWARD:         %[[#TAPE:]] = builtin "autoDiffCreateLinearMapContextWithType"
// FORWARD-NOT:     function_ref reverse-mode derivative of forwardHalvingLoop
// FORWARD-NOT:   } // end sil function
// FORWARD:         // function_ref specialized pullback of throughHalvingLoop
// FORWARD-NEXT:    %[[#FN:]] = function_ref
// FORWARD-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#FN]](%[[#TAPE]]) : $@convention(thin) (Float, @owned Builtin.NativeObject) -> Float
// FORWARD-NEXT:    %[[#RESULT:]] = tuple ({{.*}}, %[[#PB]])
// FORWARD-NEXT:    return %[[#RESULT]]

// FINAL-LABEL: {{^}}// reverse-mode derivative of throughBranchingLoop(_:_:)
// FINAL-NOT:     function_ref reverse-mode derivative of branchingLoop
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of branchingLoop(_:_:)
// FINAL-NOT:     function_ref reverse-mode derivative of branchingLoop
// FINAL:       } // end sil function

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

// Early inlining would give this loop-free wrapper a runtime loop tape
// and make its branch tape ineligible for specialization.
@differentiable(reverse, wrt: x)
public func branchingLoop(_ x: Float, _ flag: Bool) -> Float {
  if flag { return loop(x) }
  return x * x
}

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

@differentiable(reverse, wrt: x)
public func loopOverBranch(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  for _ in 0..<iterationCount {
    result = branchingLoop(result, flag)
  }
  return result
}
