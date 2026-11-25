// VJP inlining tests.

// RUN: %target-swift-frontend -emit-sil -O -verify -module-name src -Xllvm -debug-only=sil-inliner -Xllvm -sil-print-pass-name %s > %t 2>&1
// RUN: %FileCheck %s < %t
// RUN: %FileCheck %s --check-prefix=EARLY < %t

// REQUIRES: asserts
// UNSUPPORTED: OS=windows-msvc

import _Differentiation
#if canImport(Glibc)
import Glibc
#elseif canImport(Android)
import Android
#else
import Foundation
#endif

// =============================================================== //
// VJPs with control-flow are inlined into non-VJP callers
// =============================================================== //
@differentiable(reverse)
func with_control_flow(_ x: Float) -> Float {
  if (x > 0) {
    return sin(x) * cos(x)
  } else {
    return sin(x) + cos(x)
  }
}

@inline(never)
@_silgen_name("caller_of_with_control_flow")
func caller_of_with_control_flow(x: Float) -> Float {
    gradient(at: x, of: with_control_flow)
}
// CHECK-DAG: "reverse-mode derivative of src.with_control_flow(_:)" inlined into "caller_of_with_control_flow"

// =============================================================== //
// VJPs with control-flow are inlined into VJP callers
// =============================================================== //

@differentiable(reverse)
func wrapperOnWithControlFlow(x: Float) -> Float {
    return with_control_flow(x)
}
// CHECK-DAG: "reverse-mode derivative of src.with_control_flow(_:)" inlined into "reverse-mode derivative of src.wrapperOnWithControlFlow(x:)"

// =========================================================================== //
// Single-use transparent pullback adapters do not prevent early VJP inlining
// =========================================================================== //
@differentiable(reverse)
func minimum<T: Differentiable & Comparable>(_ x: T, _ y: T) -> T {
    x < y ? x : y
}

@differentiable(reverse)
func clamp(_ x: Float) -> Float {
    minimum(x > 0 ? x : 0, 1)
}

// EARLY:      Run #[[#]], stage HighLevel,Function+EarlyLoopOpt, pass [[#]]: EarlyPerfInliner (early-inline), Function: $s3src5clampyS2fFTJrSpSr
// EARLY-NOT:  stage MidLevel,Function
// EARLY-NOT:  stage LowLevel,Function
// EARLY:      decision {c={{.*}} $s3src7minimumyxx_xtSLRz16_Differentiation14DifferentiableRzlFSLRzAcDRzlTJrSSpSrSf_Tg5
// EARLY-NEXT: "specialized reverse-mode derivative of src.minimum<A>(_:_:)" inlined into "reverse-mode derivative of src.clamp(_:)"

// =============================================================== //
// Constant VJP calls can be folded in non-VJP callers
// =============================================================== //
@differentiable(reverse)
func simple(x: Float) -> Float {
    let a = x * x;
    let b = x + x;
    let c = x * a;
    let d = a + b;
    let e = b * c;

    return a * b / c + d - e ;
}

@inline(never)
@_silgen_name("caller_of_simple")
func caller_of_simple(x: Float) -> Float {
  gradient(at: Float(4), of: simple)
}
// CHECK-DAG: pure-call decision $s3src6simple1xS2f_tFTJrSpSr

// ============================================================================ //
// VJPs without control-flow are inlined into VJP callers with control-flow
// ============================================================================ //
@differentiable(reverse)
func wrapperWithControlFlowOnSimple(x: Float) -> Float {
    if (x > 0) {
        return simple(x: x) + simple(x: x)
    } else {
        return simple(x: x) * simple(x: x)
    }
}
// CHECK-DAG: "reverse-mode derivative of src.simple(x:)" inlined into "reverse-mode derivative of src.wrapperWithControlFlowOnSimple(x:)"

// =============================================================== //
// VJPs without control-flow are inlined into VJP callers
// =============================================================== //
@differentiable(reverse)
func wrapperOnSimple(x: Float) -> Float {
    return simple(x: x)
}
// CHECK-DAG: "reverse-mode derivative of src.simple(x:)" inlined into "reverse-mode derivative of src.wrapperOnSimple(x:)"
