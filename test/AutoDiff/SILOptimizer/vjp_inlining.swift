// VJP inlining tests.

// RUN: %target-swift-frontend -emit-sil -O -verify -Xllvm -debug-only=sil-inliner %s 2>&1 | %FileCheck %s

// REQUIRES: asserts
// REQUIRES: swift_in_compiler
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
// CHECK-LABEL: decision {{.*}} $s12vjp_inlining17with_control_flowyS2fFTJrSpSr
// CHECK-NEXT: "reverse-mode derivative of vjp_inlining.with_control_flow(_:)" inlined into "reverse-mode derivative of vjp_inlining.wrapperOnWithControlFlow(x:)"

// =============================================================== //
// VJPs with control-flow are inlined into VJP callers
// =============================================================== //

@differentiable(reverse)
func wrapperOnWithControlFlow(x: Float) -> Float {
    return with_control_flow(x)
}
// CHECK-LABEL: decision {{.*}} $s12vjp_inlining17with_control_flowyS2fFTJrSpSr
// CHECK-NEXT: "reverse-mode derivative of vjp_inlining.with_control_flow(_:)" inlined into "caller_of_with_control_flow"

// =============================================================== //
// VJPs without control-flow are not inlined into non-VJP callers
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
// CHECK-NOT: "reverse-mode derivative of vjp_inlining.simple(x:)" inlined into "caller_of_simple"

// ============================================================================ //
// VJPs without control-flow are not inlined into VJP callers with control-flow
// ============================================================================ //
@differentiable(reverse)
func wrapperWithControlFlowOnSimple(x: Float) -> Float {
    if (x > 0) {
        return simple(x: x) + simple(x: x)
    } else {
        return simple(x: x) * simple(x: x)
    }
}
// CHECK-NOT: "reverse-mode derivative of vjp_inlining.simple(x:)" inlined into "reverse-mode derivative of vjp_inlining.wrapperWithControlFlowOnSimple(x:)"

// =============================================================== //
// VJPs without control-flow are inlined into VJP callers
// =============================================================== //
@differentiable(reverse)
func wrapperOnSimple(x: Float) -> Float {
    return simple(x: x)
}
// CHECK-LABEL: decision {{.*}} $s12vjp_inlining6simple1xS2f_tFTJrSpSr
// CHECK-NEXT: "reverse-mode derivative of vjp_inlining.simple(x:)" inlined into "reverse-mode derivative of vjp_inlining.wrapperOnSimple(x:)"
