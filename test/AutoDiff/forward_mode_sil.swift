// RUN: %target-swift-frontend -emit-sil -Xllvm -run-jvp-generation  -verify %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -sil-print-after=differentiation -Xllvm -run-jvp-generation -o /dev/null 2>&1 %s | %FileCheck %s -check-prefix=CHECK-SIL


//===----------------------------------------------------------------------===//
// Unary
//===----------------------------------------------------------------------===//

@differentiable
@_silgen_name("unary")
func unary(_ x: Float) -> Float {
  return x * x * x
}
// CHECK-DATA-STRUCTURES: struct _AD__unary_bb0__DF__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var differential_0: (Float, Float) -> Float { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var differential_1: (Float, Float) -> Float { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__unary_bb0__Succ__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden @AD__unary__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK-SIL: bb0([[X_ARG:%.*]] : $Float):
// CHECK-SIL:   [[MULT_FUNC_1:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   retain_value [[MULT_FUNC_1]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_FUNC_JVP_1:%.*]] = function_ref @$sSf12_jvpMultiply3lhs3rhsSf5value_S2f_Sftc12differentialtSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[MULT_FUNC_VJP_1:%.*]] = function_ref @$sSf12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))
// CHECK-SIL:   [[AUTODIFF_INST_1:%.*]] = autodiff_function [wrt 0 1] [order 1] [[MULT_FUNC_1]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {[[MULT_FUNC_JVP_1]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), [[MULT_FUNC_VJP_1]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST_1:%.*]] = autodiff_function_extract [jvp] [order 1] [[AUTODIFF_INST_1]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_JVP_APPLY_TUPLE_1:%.*]] = apply [[AUTODIFF_EXTRACT_INST_1]]([[X_ARG]], [[X_ARG]], %3) : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   release_value [[AUTODIFF_INST_1]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[ORIG_RESULT_1:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE_1]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 0
// CHECK-SIL:   [[MULT_DIFF_1:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE_1]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 1
// CHECK-SIL:   [[MULT_FUNC_2:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   retain_value [[MULT_FUNC_2]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_FUNC_JVP_2:%.*]] = function_ref @$sSf12_jvpMultiply3lhs3rhsSf5value_S2f_Sftc12differentialtSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[MULT_FUNC_VJP_2:%.*]] = function_ref @$sSf12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))
// CHECK-SIL:   [[AUTODIFF_INST_2:%.*]] = autodiff_function [wrt 0 1] [order 1] [[MULT_FUNC_2]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {[[MULT_FUNC_JVP_2]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), [[MULT_FUNC_VJP_2]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST_1:%.*]] = autodiff_function_extract [jvp] [order 1] [[AUTODIFF_INST_2]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_JVP_APPLY_TUPLE_2:%.*]] = apply [[AUTODIFF_EXTRACT_INST_1]]([[ORIG_RESULT_1]], [[X_ARG]], %2) : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   release_value [[AUTODIFF_INST_2]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[ORIG_RESULT_2:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE_2]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 0
// CHECK-SIL:   [[MULT_DIFF_2:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE_2]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 1
// CHECK-SIL:   [[DIFF_STRUCT:%.*]] = struct $_AD__unary_bb0__DF__src_0_wrt_0 ([[MULT_DIFF_1]] : $@callee_guaranteed (Float, Float) -> Float, [[MULT_DIFF_2]] : $@callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[UNARY_DIFFERENTIAL:%.*]] = function_ref @AD__unary__differential_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed _AD__unary_bb0__DF__src_0_wrt_0) -> Float
// CHECK-SIL:   [[PARTIAL_APP_DIFFERENTIAL:%.*]] = partial_apply [callee_guaranteed] [[UNARY_DIFFERENTIAL]]([[DIFF_STRUCT]]) : $@convention(thin) (Float, @guaranteed _AD__unary_bb0__DF__src_0_wrt_0) -> Float
// CHECK-SIL:   [[RESULT:%.*]] = tuple ([[ORIG_RESULT_2]] : $Float, [[PARTIAL_APP_DIFFERENTIAL]] : $@callee_guaranteed (Float) -> Float)
// CHECK-SIL:   return [[RESULT]] : $(Float, @callee_guaranteed (Float) -> Float)

// CHECK-SIL-LABEL: sil hidden @AD__unary__differential_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed _AD__unary_bb0__DF__src_0_wrt_0) -> Float {
// CHECK-SIL: bb0([[X_TAN:%.*]] : $Float, [[DIFF_STRUCT:%.*]] : $_AD__unary_bb0__DF__src_0_wrt_0):
// CHECK-SIL:   retain_value [[X_TAN]] : $Float
// CHECK-SIL:   [[MULT_DIFF_1:%.*]] = struct_extract [[DIFF_STRUCT]] : $_AD__unary_bb0__DF__src_0_wrt_0, #_AD__unary_bb0__DF__src_0_wrt_0.differential_0
// CHECK-SIL:   [[TEMP_TAN_1:%.*]] = apply [[MULT_DIFF_1]]([[X_TAN]], [[X_TAN]]) : $@callee_guaranteed (Float, Float) -> Float
// CHECK-SIL:   [[MULT_DIFF_2:%.*]] = struct_extract [[DIFF_STRUCT]] : $_AD__unary_bb0__DF__src_0_wrt_0, #_AD__unary_bb0__DF__src_0_wrt_0.differential_1
// CHECK-SIL:   [[TAN_RESULT:%.*]] = apply [[MULT_DIFF_2]]([[TEMP_TAN_1]], [[X_TAN]]) : $@callee_guaranteed (Float, Float) -> Float
// CHECK-SIL:   return [[TAN_RESULT]] : $Float

//===----------------------------------------------------------------------===//
// Binary
//===----------------------------------------------------------------------===//

@differentiable
@_silgen_name("binary")
func binary(x: Float, y: Float) -> Float {
  return x * y
}

// CHECK-DATA-STRUCTURES: struct _AD__binary_bb0__DF__src_0_wrt_0_1 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var differential_0: (Float, Float) -> Float { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__binary_bb0__Succ__src_0_wrt_0_1 {
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden @AD__binary__jvp_src_0_wrt_0_1 : $@convention(thin) (Float, Float) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float) {
// CHECK-SIL: bb0([[X_ARG:%.*]] : $Float, %1 : $Float):
// CHECK-SIL:   [[MULT_FUNC:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   retain_value [[MULT_FUNC]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_FUNC_JVP:%.*]] = function_ref @$sSf12_jvpMultiply3lhs3rhsSf5value_S2f_Sftc12differentialtSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[MULT_FUNC_VJP:%.*]] = function_ref @$sSf12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktSf_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))
// CHECK-SIL:   [[AUTODIFF_INST:%.*]] = autodiff_function [wrt 0 1] [order 1] [[MULT_FUNC]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {[[MULT_FUNC_JVP]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float), [[MULT_FUNC_VJP]] : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float))}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST:%.*]] = autodiff_function_extract [jvp] [order 1] [[AUTODIFF_INST]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[MULT_JVP_APPLY_TUPLE:%.*]] = apply [[AUTODIFF_EXTRACT_INST]]([[X_ARG]], %1, %4) : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   release_value [[AUTODIFF_INST]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[ORIG_RESULT:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 0
// CHECK-SIL:   [[MULT_DIFF:%.*]] = tuple_extract [[MULT_JVP_APPLY_TUPLE]] : $(Float, @callee_guaranteed (Float, Float) -> Float), 1
// CHECK-SIL:   [[DIFF_STRUCT:%.*]] = struct $_AD__binary_bb0__DF__src_0_wrt_0_1 ([[MULT_DIFF]] : $@callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[BINARY_DIFFERENTIAL:%.*]] = function_ref @AD__binary__differential_src_0_wrt_0_1 : $@convention(thin) (Float, Float, @guaranteed _AD__binary_bb0__DF__src_0_wrt_0_1) -> Float
// CHECK-SIL:   [[PARTIAL_APP_DIFFERENTIAL:%.*]] = partial_apply [callee_guaranteed] [[BINARY_DIFFERENTIAL]]([[DIFF_STRUCT]]) : $@convention(thin) (Float, Float, @guaranteed _AD__binary_bb0__DF__src_0_wrt_0_1) -> Float
// CHECK-SIL:   [[RESULT:%.*]] = tuple ([[ORIG_RESULT]] : $Float, [[PARTIAL_APP_DIFFERENTIAL]] : $@callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   return [[RESULT:%.*]] : $(Float, @callee_guaranteed (Float, Float) -> Float)

// CHECK-SIL-LABEL: sil hidden @AD__binary__differential_src_0_wrt_0_1 : $@convention(thin) (Float, Float, @guaranteed _AD__binary_bb0__DF__src_0_wrt_0_1) -> Float {
// CHECK-SIL: bb0([[X_TAN:%.*]] : $Float, [[Y_TAN:%.*]] : $Float, [[DIFF_STRUCT:%.*]] : $_AD__binary_bb0__DF__src_0_wrt_0_1):
// CHECK-SIL:   [[MULT_DIFF:%.*]] = struct_extract [[DIFF_STRUCT]] : $_AD__binary_bb0__DF__src_0_wrt_0_1, #_AD__binary_bb0__DF__src_0_wrt_0_1.differential_0
// CHECK-SIL:   [[TAN_RESULT:%.*]] = apply [[MULT_DIFF]]([[X_TAN]], [[Y_TAN]]) : $@callee_guaranteed (Float, Float) -> Float
// CHECK-SIL:   return [[TAN_RESULT]] : $Float
