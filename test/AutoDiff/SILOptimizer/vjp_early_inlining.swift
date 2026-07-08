// REQUIRES: asserts

// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=sil-inliner -Xllvm -sil-print-pass-name %s -o %t.sil 2> %t.log

// RUN: cat %t.log | %FileCheck %s --check-prefix=CHECK1
// RUN: cat %t.log | %FileCheck %s --check-prefix=CHECK2

// RUN: cat %t.sil | %FileCheck %s --check-prefix=CHECK3

import _Differentiation

/// Only top-level pullback => VJP is assumed trivial in terms of VJP inlining decision taking
@differentiable(reverse)
func trivial(_ x: Float) -> Float { x * x }

public func nonVjpCallerOfTrivial(_ x: Float) -> Float {
    gradient(at: x, of: trivial)
}

// CHECK1:       Run #[[#]], stage MidLevel,Function, pass [[#]]: PerfInliner (inline), Function: $s4main21nonVjpCallerOfTrivialyS2fF
// CHECK1:       Inlining calls into $s4main21nonVjpCallerOfTrivialyS2fF
// CHECK1-EMPTY:
// CHECK1-NEXT:  Inline into caller: $s4main21nonVjpCallerOfTrivialyS2fF
// CHECK1-NOT:   Run #[[#]], stage LowLevel,Function, pass [[#]]: PerfInliner (inline), Function: $s4main21nonVjpCallerOfTrivialyS2fF
// CHECK1:           decision {{.*}} $s4main7trivialyS2fFTJrSpSr
// CHECK1-NEXT:  "reverse-mode derivative of main.trivial(_:)" inlined into "main.nonVjpCallerOfTrivial(_:)"


@differentiable(reverse)
@inline(never)
func mul2(_ x: Float, _ y: Float) -> Float { x * y }

/// Make VJP of `square` non-trivial by forcing call to `@inline(never)` VJP of `mul2`
/// in order to test further inlining decision logic.
@differentiable(reverse)
func square(_ x: Float) -> Float { mul2(x, x) }

@differentiable(reverse)
public func vjpCallerWithControlFlow(_ x: Float) -> Float {
    let y: Float
    if x > 0 {
      y = 37
    } else {
      y = 42
    }
    return y * square(x)
}

// CHECK2:       Run #[[#]], stage HighLevel,Function+EarlyLoopOpt, pass [[#]]: EarlyPerfInliner (early-inline), Function: $s4main24vjpCallerWithControlFlowyS2fFTJrSpSr
// CHECK2:       Inlining calls into $s4main24vjpCallerWithControlFlowyS2fFTJrSpSr
// CHECK2-EMPTY:
// CHECK2-NEXT:  Inline into caller: $s4main24vjpCallerWithControlFlowyS2fFTJrSpSr
// CHECK2-NOT:   Run #[[#]], stage MidLevel,Function, pass [[#]]: PerfInliner (inline), Function: $s4main24vjpCallerWithControlFlowyS2fFTJrSpSr
// CHECK2-NOT:   Run #[[#]], stage LowLevel,Function, pass [[#]]: PerfInliner (inline), Function: $s4main24vjpCallerWithControlFlowyS2fFTJrSpSr
// CHECK2:           decision {{.*}} $s4main6squareyS2fFTJrSpSr
// CHECK2-NEXT:  "reverse-mode derivative of main.square(_:)" inlined into "reverse-mode derivative of main.vjpCallerWithControlFlow(_:)"


/// Ensure that early inlining of VJP of `square` into VJP of `vjpCallerWithControlFlow` unblocks ADCS and
/// no nested pullback closure is passed to the specialized pullback of `vjpCallerWithControlFlow`

// CHECK3:      // reverse-mode derivative of vjpCallerWithControlFlow(_:)
// CHECK3-NEXT: // Isolation: unspecified
// CHECK3-NEXT: sil @$s4main24vjpCallerWithControlFlowyS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK3:        // function_ref specialized pullback of vjpCallerWithControlFlow(_:)
// CHECK3-NEXT:   %[[#F:]] = function_ref @$s4main24vjpCallerWithControlFlowyS2fFTJpSpSr26$s4main6squareyS2fFTJpSpSrS3fIegydd_026$sSf16_DifferentiationE12_b44Multiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktr1_s5FZSf_S6SfcfU_S2fTf1nnEE_n013$s4main4mul2yh1_si3SSpK0S3fIegydd_Tf1nnEnn_nADS2fTf1nnnnE_n : $@convention(thin) (Float, @owned _AD__$s4main24vjpCallerWithControlFlowyS2fF_bb3__Pred__src_0_wrt_0, Float, Float, Float, Float) -> Float
// CHECK3-NEXT:   partial_apply [callee_guaranteed] %[[#F]](%[[#]], %[[#]], %[[#]], %0, %0) : $@convention(thin) (Float, @owned _AD__$s4main24vjpCallerWithControlFlowyS2fF_bb3__Pred__src_0_wrt_0, Float, Float, Float, Float) -> Float
// CHECK3:      } // end sil function '$s4main24vjpCallerWithControlFlowyS2fFTJrSpSr'
