// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -run-jvp-generation -Xllvm -debug-only=differentiation %s 2>&1 | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
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
// CHECK-DATA-STRUCTURES:   var differential_0: (Float, Float) -> Float
// CHECK-DATA-STRUCTURES:   var differential_1: (Float, Float) -> Float
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__unary_bb0__Succ__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__unary__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) {
// CHECK-SIL: bb0([[X_ARG:%.*]] : $Float):
// CHECK-SIL:   [[FUNC_1:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[JVP_1:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float)
// CHECK-SIL:   [[VJP_1:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__vjp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @out Float))
// CHECK-SIL:   [[AUTODIFF_INST_1:%.*]] = differentiable_function [wrt 0 1] [order 1] [[FUNC_1]] : {{.*}} with {[[JVP_1]] : {{.*}}, [[VJP_1]] : {{.*}}}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST_1:%.*]] = differentiable_function_extract [jvp] [order 1] [[AUTODIFF_INST_1]]
// CHECK-SIL:   [[JVP_APPLY_TUPLE_1:%.*]] = apply [[AUTODIFF_EXTRACT_INST_1]]([[X_ARG]], [[X_ARG]], {{.*}})
// CHECK-SIL:   ([[ORIG_RESULT_1:%.*]], [[DIFF_1:%.*]]) = destructure_tuple [[JVP_APPLY_TUPLE_1]]
// CHECK-SIL:   [[DIFF_1_THUNK:%.*]] = function_ref @$sS3fIegnnr_S3fIegyyd_TR : $@convention(thin) (Float, Float, @guaranteed @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float) -> Float
// CHECK-SIL:   [[DIFF_1_THUNKED:%.*]] = partial_apply [callee_guaranteed] [[DIFF_1_THUNK]]([[DIFF_1]])
// CHECK-SIL:   [[FUNC_2:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[JVP_2:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float)
// CHECK-SIL:   [[VJP_2:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__vjp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @out Float))
// CHECK-SIL:   [[AUTODIFF_INST_2:%.*]] = differentiable_function [wrt 0 1] [order 1] [[FUNC_2]] : {{.*}} with {[[JVP_2]] : {{.*}}, [[VJP_2]] : {{.*}}}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST_1:%.*]] = differentiable_function_extract [jvp] [order 1] [[AUTODIFF_INST_2]]
// CHECK-SIL:   [[JVP_APPLY_TUPLE_2:%.*]] = apply [[AUTODIFF_EXTRACT_INST_1]]([[ORIG_RESULT_1]], [[X_ARG]], {{.*}})
// CHECK-SIL:   ([[ORIG_RESULT_2:%.*]], [[DIFF_2:%.*]]) = destructure_tuple [[JVP_APPLY_TUPLE_2]]
// CHECK-SIL:   [[DIFF_2_THUNK:%.*]] = function_ref @$sS3fIegnnr_S3fIegyyd_TR : $@convention(thin) (Float, Float, @guaranteed @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float) -> Float
// CHECK-SIL:   [[DIFF_2_THUNKED:%.*]] = partial_apply [callee_guaranteed] [[DIFF_2_THUNK]]([[DIFF_2]])
// CHECK-SIL:   [[DIFF_STRUCT:%.*]] = struct $_AD__unary_bb0__DF__src_0_wrt_0 ([[DIFF_1_THUNKED]] : {{.*}}, [[DIFF_2_THUNKED]] : {{.*}})
// CHECK-SIL:   [[UNARY_DIFFERENTIAL:%.*]] = function_ref @AD__unary__differential_src_0_wrt_0 : $@convention(thin) (@in_guaranteed Float, @owned _AD__unary_bb0__DF__src_0_wrt_0) -> @out Float
// CHECK-SIL:   [[PARTIAL_APP_DIFFERENTIAL:%.*]] = partial_apply [callee_guaranteed] [[UNARY_DIFFERENTIAL]]([[DIFF_STRUCT]])
// CHECK-SIL:   [[RESULT:%.*]] = tuple ([[ORIG_RESULT_2]] : $Float, [[PARTIAL_APP_DIFFERENTIAL]] : $@callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK-SIL:   return [[RESULT]]

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__unary__differential_src_0_wrt_0 : $@convention(thin) (@in_guaranteed Float, @owned _AD__unary_bb0__DF__src_0_wrt_0) -> @out Float {
// CHECK-SIL: bb0([[TAN_RESULT:%.*]] : $*Float, [[TAN_X:%.*]] : $*Float, [[DIFF_STRUCT:%.*]] : @owned $_AD__unary_bb0__DF__src_0_wrt_0):
// CHECK-SIL:   [[TAN_RESULT_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[TAN_TMP_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[TAN_X_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   copy_addr [[TAN_X]] to [initialization] [[TAN_X_BUF]]
// CHECK-SIL:   ([[DIFF_1:%.*]], [[DIFF_2:%.*]]) = destructure_struct [[DIFF_STRUCT]]
// CHECK-SIL:   [[DIFF_1_THUNKED:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[DIFF_1]])
// CHECK-SIL:   apply [[DIFF_1_THUNKED]]([[TAN_TMP_BUF]], [[TAN_X_BUF]], [[TAN_X_BUF]])
// CHECK-SIL:   [[DIFF_2_THUNKED:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[DIFF_2]])
// CHECK-SIL:   apply [[DIFF_2_THUNKED]]([[TAN_RESULT_BUF]], [[TAN_TMP_BUF]], [[TAN_X_BUF]])
// CHECK-SIL:   copy_addr [take] [[TAN_RESULT_BUF]] to [initialization] [[TAN_RESULT]]
// CHECK-SIL:   [[VOID:%.*]] = tuple ()
// CHECK-SIL:   return [[VOID]]

//===----------------------------------------------------------------------===//
// Binary
//===----------------------------------------------------------------------===//

@differentiable
@_silgen_name("binary")
func binary(x: Float, y: Float) -> Float {
  return x * y
}

// CHECK-DATA-STRUCTURES: struct _AD__binary_bb0__DF__src_0_wrt_0_1 {
// CHECK-DATA-STRUCTURES:   var differential_0: (Float, Float) -> Float
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__binary_bb0__Succ__src_0_wrt_0_1 {
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__binary__jvp_src_0_wrt_0_1 : $@convention(thin) (Float, Float) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float) {
// CHECK-SIL: bb0([[X_ARG:%.*]] : $Float, [[Y_ARG:%.*]] : $Float):
// CHECK-SIL:   [[FUNC:%.*]] = function_ref @$sSf1moiyS2f_SftFZ : $@convention(method) (Float, Float, @thin Float.Type) -> Float
// CHECK-SIL:   [[JVP:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__jvp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float)
// CHECK-SIL:   [[VJP:%.*]] = function_ref @AD__$sSf1moiyS2f_SftFZ__vjp_src_0_wrt_0_1 : $@convention(method) (Float, Float, @thin Float.Type) -> (Float, @owned @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @out Float))
// CHECK-SIL:   [[AUTODIFF_INST:%.*]] = differentiable_function [wrt 0 1] [order 1] [[FUNC]] : $@convention(method) (Float, Float, @thin Float.Type) -> Float with {[[JVP]] : {{.*}}, [[VJP]] : {{.*}})}
// CHECK-SIL:   [[AUTODIFF_EXTRACT_INST:%.*]] = differentiable_function_extract [jvp] [order 1] [[AUTODIFF_INST]] : $@differentiable @convention(method) (Float, Float, @nondiff @thin Float.Type) -> Float
// CHECK-SIL:   [[JVP_APPLY_TUPLE:%.*]] = apply [[AUTODIFF_EXTRACT_INST]]([[X_ARG]], [[Y_ARG]], {{.*}})
// CHECK-SIL:   ([[ORIG_RESULT:%.*]], [[DIFF_1:%.*]]) = destructure_tuple [[JVP_APPLY_TUPLE]]
// CHECK-SIL:   [[DIFF_1_THUNKED:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[DIFF_1]])
// CHECK-SIL:   [[DIFF_STRUCT:%.*]] = struct $_AD__binary_bb0__DF__src_0_wrt_0_1 ([[DIFF_1_THUNKED]] : $@callee_guaranteed (Float, Float) -> Float)
// CHECK-SIL:   [[BINARY_DIFFERENTIAL:%.*]] = function_ref @AD__binary__differential_src_0_wrt_0_1 : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float, @owned _AD__binary_bb0__DF__src_0_wrt_0_1) -> @out Float
// CHECK-SIL:   [[PARTIAL_APP_DIFFERENTIAL:%.*]] = partial_apply [callee_guaranteed] [[BINARY_DIFFERENTIAL]]([[DIFF_STRUCT]])
// CHECK-SIL:   [[RESULT:%.*]] = tuple ([[ORIG_RESULT]] : $Float, [[PARTIAL_APP_DIFFERENTIAL]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float)
// CHECK-SIL:   return [[RESULT:%.*]]

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__binary__differential_src_0_wrt_0_1 : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float, @owned _AD__binary_bb0__DF__src_0_wrt_0_1) -> @out Float {
// CHECK-SIL: bb0([[TAN_RESULT]] : $*Float, [[TAN_X:%.*]] : $*Float, [[TAN_Y:%.*]] : $*Float, [[DIFF_STRUCT:%.*]] : @owned $_AD__binary_bb0__DF__src_0_wrt_0_1):
// CHECK-SIL:   [[TAN_RESULT_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[TAN_Y_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   copy_addr [[TAN_Y]] to [initialization] [[TAN_Y_BUF]]
// CHECK-SIL:   [[TAN_X_BUF:%.*]] = alloc_stack $Float
// CHECK-SIL:   copy_addr [[TAN_X]] to [initialization] [[TAN_X_BUF]]
// CHECK-SIL:   [[DIFF_1:%.*]] = destructure_struct [[DIFF_STRUCT]]
// CHECK-SIL:   [[DIFF_1_THUNKED:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[DIFF_1]])
// CHECK-SIL:   apply [[DIFF_1_THUNKED]]([[TAN_RESULT_BUF]], [[TAN_X_BUF]], [[TAN_Y_BUF]])
// CHECK-SIL:   copy_addr [take] [[TAN_RESULT_BUF]] to [initialization] [[TAN_RESULT]]
// CHECK-SIL:   [[VOID:%.*]] = tuple ()
// CHECK-SIL:   return [[VOID]]
