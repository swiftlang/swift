// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -enable-experimental-forward-mode-differentiation -verify -Xllvm -sil-print-after=differentiation -o /dev/null 2>&1 %s | %FileCheck %s -check-prefix=CHECK-SIL
// REQUIRES: asserts

// Simple generated derivative code FileCheck tests.

import _Differentiation

extension Float {
  @_silgen_name("add")
  static func add(_ x: Float, _ y: Float) -> Float {
    return x + y
  }

  @derivative(of: add)
  static func addVJP(_ x: Float, _ y: Float) -> (
    value: Float, pullback: (Float) -> (Float, Float)
  ) {
    return (add(x, y), { v in (v, v) })
  }
}

@_silgen_name("foo")
@differentiable(reverse)
func foo(_ x: Float) -> Float {
  let y = Float.add(x, x)
  return y
}

// CHECK-SIL-LABEL: enum _AD__foo_bb0__Pred__src_0_wrt_0 {
// CHECK-SIL-NEXT:  }

// CHECK-SIL-LABEL: enum _AD__fooMethod_bb0__Pred__src_0_wrt_0 {
// CHECK-SIL-NEXT:  }

// CHECK-SIL-LABEL: sil hidden [ossa] @fooTJfSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK-SIL: bb0([[X:%.*]] : $Float):
// CHECK-SIL:   [[ADD_ORIG_REF:%.*]] = function_ref @add : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[ADD_JVP_REF:%.*]] = differentiability_witness_function [jvp] [reverse] [parameters 0 1] [results 0] @add
// CHECK-SIL:   [[ADD_VJP_REF:%.*]] = differentiability_witness_function [vjp] [reverse] [parameters 0 1] [results 0] @add
// CHECK-SIL:   [[ADD_DIFF_FN:%.*]] = differentiable_function [parameters 0 1] [results 0] [[ADD_ORIG_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with_derivative {[[ADD_JVP_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), [[ADD_VJP_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK-SIL:   [[ADD_JVP_FN:%.*]] = differentiable_function_extract [jvp] [[ADD_DIFF_FN]]
// CHECK-SIL:   [[ADD_RESULT:%.*]] = apply [[ADD_JVP_FN]]([[X]], [[X]], {{.*}})
// CHECK-SIL:   ([[ORIG_RES:%.*]], [[ADD_DF:%.*]]) = destructure_tuple [[ADD_RESULT]]
// CHECK-SIL:   [[MV_RES:%.*]] = move_value [var_decl] [[ORIG_RES]] : $Float
// CHECK-SIL:   [[DF_STRUCT:%.*]] = tuple ([[ADD_DF]] : $@callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[DF_REF:%.*]] = function_ref @fooTJdSpSr : $@convention(thin) (Float, @owned (_: @callee_guaranteed (Float, Float) -> Float)) -> Float
// CHECK-SIL:   [[DF_FN:%.*]] = partial_apply [callee_guaranteed] [[DF_REF]]([[DF_STRUCT]])
// CHECK-SIL:   [[VJP_RESULT:%.*]] = tuple ([[MV_RES]] : $Float, [[DF_FN]] : $@callee_guaranteed (Float) -> Float)
// CHECK-SIL:   return [[VJP_RESULT]] : $(Float, @callee_guaranteed (Float) -> Float)
// CHECK-SIL: }

// CHECK-SIL-LABEL: sil private [ossa] @fooTJdSpSr : $@convention(thin) (Float, @owned (_: @callee_guaranteed (Float, Float) -> Float)) -> Float {
// CHECK-SIL: bb0([[DX:%.*]] : $Float, [[DF_STRUCT:%.*]] : @owned $(_: @callee_guaranteed (Float, Float) -> Float)):
// CHECK-SIL:   [[ADD_DF:%.*]] = destructure_tuple [[DF_STRUCT]] : $(_: @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[DY:%.*]] = apply [[ADD_DF]]([[DX]], [[DX]]) : $@callee_guaranteed (Float, Float) -> Float
// CHECK-SIL:   destroy_value [[ADD_DF]] : $@callee_guaranteed (Float, Float) -> Float
// CHECK-SIL:   return [[DY]] : $Float
// CHECK-SIL: }

// CHECK-SIL-LABEL: sil hidden [ossa] @fooTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK-SIL: bb0([[X:%.*]] : $Float):
// CHECK-SIL:   [[ADD_ORIG_REF:%.*]] = function_ref @add : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[ADD_JVP_REF:%.*]] = differentiability_witness_function [jvp] [reverse] [parameters 0 1] [results 0] @add
// CHECK-SIL:   [[ADD_VJP_REF:%.*]] = differentiability_witness_function [vjp] [reverse] [parameters 0 1] [results 0] @add
// CHECK-SIL:   [[ADD_DIFF_FN:%.*]] = differentiable_function [parameters 0 1] [results 0] [[ADD_ORIG_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with_derivative {[[ADD_JVP_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), [[ADD_VJP_REF]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK-SIL:   [[ADD_VJP_FN:%.*]] = differentiable_function_extract [vjp] [[ADD_DIFF_FN]]
// CHECK-SIL:   [[ADD_RESULT:%.*]] = apply [[ADD_VJP_FN]]([[X]], [[X]], {{.*}})
// CHECK-SIL:   ([[ORIG_RES:%.*]], [[ADD_PB:%.*]]) = destructure_tuple [[ADD_RESULT]]
// CHECK-SIL:   [[PB_REF:%.*]] = function_ref @fooTJpSpSr : $@convention(thin) (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) -> Float
// CHECK-SIL:   [[PB_FN:%.*]] = partial_apply [callee_guaranteed] [[PB_REF]]([[ADD_PB]])
// CHECK-SIL:   [[VJP_RESULT:%.*]] = tuple ([[MV_RES]] : $Float, [[PB_FN]] : $@callee_guaranteed (Float) -> Float)
// CHECK-SIL:   return [[VJP_RESULT]] : $(Float, @callee_guaranteed (Float) -> Float)
// CHECK-SIL: }

// CHECK-SIL-LABEL: sil private [ossa] @fooTJpSpSr : $@convention(thin) (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) -> Float {
// CHECK-SIL: bb0([[DY:%.*]] : $Float, [[ADD_PB:%.*]] : @owned $@callee_guaranteed (Float) -> (Float, Float)):
// CHECK-SIL:   debug_value [[DY]] : $Float, let, name "y"
// CHECK-SIL:   [[ADD_PB_RES:%.*]] = apply [[ADD_PB]]([[DY]]) : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK-SIL:   ([[DX_1:%.*]], [[DX_2:%.*]]) = destructure_tuple [[ADD_PB_RES]] : $(Float, Float)
// CHECK-SIL:   [[TMP_BUF_RES:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[TMP_BUF_LHS:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[TMP_BUF_RHS:%.*]] = alloc_stack $Float
// CHECK-SIL:   store [[DX_1]] to [trivial] [[TMP_BUF_LHS]] : $*Float
// CHECK-SIL:   store [[DX_2]] to [trivial] [[TMP_BUF_RHS]] : $*Float
// CHECK-SIL:   [[PLUS_FN:%.*]] = witness_method $Float, #AdditiveArithmetic."+"
// CHECK-SIL:   apply [[PLUS_FN]]<Float>([[TMP_BUF_RES]], [[TMP_BUF_RHS]], [[TMP_BUF_LHS]], {{.*}})
// CHECK-SIL:   destroy_addr [[TMP_BUF_LHS]] : $*Float
// CHECK-SIL:   destroy_addr [[TMP_BUF_RHS]] : $*Float
// CHECK-SIL:   dealloc_stack [[TMP_BUF_RHS]] : $*Float
// CHECK-SIL:   dealloc_stack [[TMP_BUF_LHS]] : $*Float
// CHECK-SIL:   [[DX:%.*]] = load [trivial] [[TMP_BUF_RES]] : $*Float
// CHECK-SIL:   dealloc_stack [[TMP_BUF_RES]] : $*Float
// CHECK-SIL:   debug_value [[DX]] : $Float, let, name "x", argno 1
// CHECK-SIL:   return [[DX]] : $Float
// CHECK-SIL: }

// https://github.com/apple/swift/issues/56342
// Check the conventions of the generated functions for a method.
struct ExampleStruct {
  @_silgen_name("fooMethod")
  @differentiable(reverse)
  func fooMethod(_ x: Float) -> Float {
    let y = Float.add(x, x)
    return y
  }
}

// CHECK-SIL-LABEL: sil hidden [ossa] @fooMethodTJfSUpSr  : $@convention(method) (Float, ExampleStruct) -> (Float, @owned @callee_guaranteed (Float) -> Float) {

// CHECK-SIL-LABEL: sil private [ossa] @fooMethodTJdSUpSr : $@convention(thin) (Float, @owned (_: @callee_guaranteed (Float, Float) -> Float)) -> Float {

// CHECK-SIL-LABEL: sil hidden [ossa] @fooMethodTJrSUpSr : $@convention(method) (Float, ExampleStruct) -> (Float, @owned @callee_guaranteed (Float) -> Float) {

// CHECK-SIL-LABEL: sil private [ossa] @fooMethodTJpSUpSr : $@convention(thin) (Float, @owned @callee_guaranteed (Float) -> (Float, Float)) -> Float {
