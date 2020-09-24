// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil %s | %FileCheck %s --check-prefix=CHECK-SIL

// Simple differentiation transform test: check SIL before and after the transform.

import _Differentiation

@_silgen_name("basic")
@differentiable
func basic(_ x: Float) -> Float { x }

// Test differentiability witnesses.

// CHECK-SILGEN-LABEL: sil_differentiability_witness hidden [parameters 0] [results 0] @basic : $@convention(thin) (Float) -> Float {
// CHECK-SILGEN-NEXT: }

// CHECK-SIL-LABEL: sil_differentiability_witness hidden [parameters 0] [results 0] @basic : $@convention(thin) (Float) -> Float {
// CHECK-SIL-NEXT:   jvp: @AD__basic__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SIL-NEXT:   vjp: @AD__basic__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-SIL-NEXT: }

// Test `differentiable_function` instructions.

@_silgen_name("test_differentiable_function")
func testDifferentiableFunction() {
  let _: @differentiable (Float) -> Float = basic
}

// CHECK-SILGEN-LABEL: sil hidden [ossa] @test_differentiable_function : $@convention(thin) () -> () {
// CHECK-SILGEN:   [[ORIG_FN_REF:%.*]] = function_ref @basic : $@convention(thin) (Float) -> Float
// CHECK-SILGEN:   [[ORIG_FN:%.*]] = thin_to_thick_function [[ORIG_FN_REF]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[DIFF_FN:%.*]] = differentiable_function [parameters 0] [results 0] [[ORIG_FN]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN: }

// CHECK-SIL-LABEL: sil hidden @test_differentiable_function : $@convention(thin) () -> () {
// CHECK-SIL:   [[ORIG_FN_REF:%.*]] = function_ref @basic : $@convention(thin) (Float) -> Float
// CHECK-SIL:   [[ORIG_FN:%.*]] = thin_to_thick_function [[ORIG_FN_REF]]
// CHECK-SIL:   [[JVP_FN_REF:%.*]] = differentiability_witness_function [jvp] [parameters 0] [results 0] @basic
// CHECK-SIL:   [[JVP_FN:%.*]] = thin_to_thick_function [[JVP_FN_REF]]
// CHECK-SIL:   [[VJP_FN_REF:%.*]] = differentiability_witness_function [vjp] [parameters 0] [results 0] @basic
// CHECK-SIL:   [[VJP_FN:%.*]] = thin_to_thick_function [[VJP_FN_REF]]
// CHECK-SIL:   [[DIFF_FN:%.*]] = differentiable_function [parameters 0] [results 0] [[ORIG_FN]] : $@callee_guaranteed (Float) -> Float with_derivative {[[JVP_FN]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float), [[VJP_FN]] : $@callee_guaranteed (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)}
// CHECK-SIL: }
