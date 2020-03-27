// RUN: %target-swift-frontend -parse-stdlib -emit-silgen -enable-experimental-differentiable-programming %s | %FileCheck %s

import Swift
import _Differentiation

@_silgen_name("f_direct_arity1")
func f_direct_arity1(_ x: Float) -> Float {
  x
}

@_silgen_name("f_direct_arity1_jvp")
func f_direct_arity1_jvp(_ x: Float) -> (Float, (Float) -> Float) {
  (x, { $0 })
}

@_silgen_name("f_direct_arity1_vjp")
func f_direct_arity1_vjp(_ x: Float) -> (Float, (Float) -> Float) {
  (x, { $0 })
}

@_silgen_name("f_direct_arity2")
func f_direct_arity2(_ x: Float, _ y: Float) -> Float {
  x
}

@_silgen_name("f_indirect_arity1")
func f_indirect_arity1<T: AdditiveArithmetic & Differentiable>(_ x: T) -> T {
  x
}

// MARK: - applyDerivative

@_silgen_name("applyDerivative_f_direct_arity1_jvp")
func applyDerivative_f1_jvp(_ x: Float) -> (Float, (Float) -> Float) {
  return Builtin.applyDerivative_jvp(f_direct_arity1, x)
}
// CHECK-LABEL: sil{{.*}}@applyDerivative_f_direct_arity1_jvp
// CHECK: bb0([[X:%.*]] : $Float):
// CHECK: [[D:%.*]] = differentiable_function_extract [jvp]
// CHECK: [[D_RESULT:%.*]] = apply [[D]]([[X]])
// CHECK: ([[D_RESULT_0:%.*]], [[D_RESULT_1:%.*]]) = destructure_tuple [[D_RESULT]]
// CHECK: [[D_RESULT_RETUPLED:%.*]] = tuple ([[D_RESULT_0]] : {{.*}}, [[D_RESULT_1]] : {{.*}})
// CHECK: return [[D_RESULT_RETUPLED]]

@_silgen_name("applyDerivative_f_direct_arity1_vjp")
func applyDerivative_f1_vjp(_ x: Float) -> (Float, (Float) -> Float) {
  return Builtin.applyDerivative_vjp(f_direct_arity1, x)
}
// CHECK-LABEL: sil{{.*}}@applyDerivative_f_direct_arity1_vjp
// CHECK: bb0([[X:%.*]] : $Float):
// CHECK: [[D:%.*]] = differentiable_function_extract [vjp]
// CHECK: [[D_RESULT:%.*]] = apply [[D]]([[X]])
// CHECK: ([[D_RESULT_0:%.*]], [[D_RESULT_1:%.*]]) = destructure_tuple [[D_RESULT]]
// CHECK: [[D_RESULT_RETUPLED:%.*]] = tuple ([[D_RESULT_0]] : {{.*}}, [[D_RESULT_1]] : {{.*}})
// CHECK: return [[D_RESULT_RETUPLED]]

@_silgen_name("applyDerivative_f_direct_arity2_vjp")
func applyDerivative_f1_vjp(_ x: Float, _ y: Float) -> (Float, (Float) -> (Float, Float)) {
  return Builtin.applyDerivative_vjp_arity2(f_direct_arity2, x, y)
}
// CHECK-LABEL: sil{{.*}}@applyDerivative_f_direct_arity2_vjp
// CHECK: bb0([[X:%.*]] : $Float, [[Y:%.*]] : $Float):
// CHECK: [[D:%.*]] = differentiable_function_extract [vjp]
// CHECK: [[D_RESULT:%.*]] = apply [[D]]([[X]], [[Y]])
// CHECK: ([[D_RESULT_0:%.*]], [[D_RESULT_1:%.*]]) = destructure_tuple [[D_RESULT]]
// CHECK: [[D_RESULT_RETUPLED:%.*]] = tuple ([[D_RESULT_0]] : {{.*}}, [[D_RESULT_1]] : {{.*}})
// CHECK: return [[D_RESULT_RETUPLED]]

@_silgen_name("applyDerivative_f_indirect_arity1_vjp")
func applyDerivative_f1_vjp<T: AdditiveArithmetic & Differentiable>(t0: T) -> (T, (T.TangentVector) -> T.TangentVector) {
  return Builtin.applyDerivative_vjp(f_indirect_arity1, t0)
}
// CHECK-LABEL: sil{{.*}}@applyDerivative_f_indirect_arity1_vjp
// CHECK: bb0([[ORIG_RESULT_OUT_PARAM:%.*]] : $*T, [[X:%.]] : $*T):
// CHECK: [[D:%.*]] = differentiable_function_extract [vjp]
// CHECK: [[D_RESULT_BUFFER:%.*]] = alloc_stack $(T, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T.TangentVector, T.TangentVector>)
// CHECK: [[D_RESULT_BUFFER_0_FOR_STORE:%.*]] = tuple_element_addr [[D_RESULT_BUFFER]] : ${{.*}}, 0
// CHECK: [[D_RESULT:%.*]] = apply [[D]]([[D_RESULT_BUFFER_0_FOR_STORE]], [[X]])
// CHECK: [[D_RESULT_BUFFER_1_FOR_STORE:%.*]] = tuple_element_addr [[D_RESULT_BUFFER]] : ${{.*}}, 1
// CHECK: store [[D_RESULT]] to [init] [[D_RESULT_BUFFER_1_FOR_STORE]]
// CHECK: [[D_RESULT_BUFFER_0_FOR_LOAD:%.*]] = tuple_element_addr [[D_RESULT_BUFFER]] : ${{.*}}, 0
// CHECK: [[D_RESULT_BUFFER_1_FOR_LOAD:%.*]] = tuple_element_addr [[D_RESULT_BUFFER]] : ${{.*}}, 1
// CHECK: [[PULLBACK:%.*]] = load [take] [[D_RESULT_BUFFER_1_FOR_LOAD]]
// CHECK: copy_addr [take] [[D_RESULT_BUFFER_0_FOR_LOAD]] to [initialization] [[ORIG_RESULT_OUT_PARAM]]
// CHECK: return [[PULLBACK]]

// MARK: - applyTranspose

@_silgen_name("applyTranspose_f_direct_arity1")
func applyTranspose_f_direct_arity1(_ x: Float) -> Float {
  return Builtin.applyTranspose_arity1(f_direct_arity1, x)
}
// CHECK-LABEL: sil{{.*}}@applyTranspose_f_direct_arity1
// CHECK: bb0([[X:%.*]] : $Float):
// CHECK: [[ORIG_FN_THIN:%.*]] = function_ref @f_direct_arity1 : $@convention(thin) (Float) -> Float
// CHECK: [[ORIG_FN:%.*]] = thin_to_thick_function [[ORIG_FN_THIN]]
// CHECK: [[LINEAR_FN_ESCAPING:%.*]] = linear_function [parameters 0] [[ORIG_FN]]
// CHECK: [[LINEAR_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[LINEAR_FN_ESCAPING]]
// CHECK: [[TRANSPOSE_FN:%.*]] = linear_function_extract [transpose] [[LINEAR_FN]]
// CHECK: [[RESULT:%.*]] = apply [[TRANSPOSE_FN]]([[X]]) : $@noescape @callee_guaranteed (Float) -> Float
// CHECK: destroy_value [[LINEAR_FN_ESCAPING]]
// CHECK: return [[RESULT]]

@_silgen_name("applyTranspose_f_direct_arity2")
func applyTranspose_f_direct_arity2(_ x: Float) -> (Float, Float) {
  return Builtin.applyTranspose_arity2(f_direct_arity2, x)
}
// CHECK-LABEL: sil{{.*}}@applyTranspose_f_direct_arity2
// CHECK: bb0([[X:%.*]] : $Float)
// CHECK: [[ORIG_FN_THIN:%.*]] = function_ref @f_direct_arity2 : $@convention(thin) (Float, Float) -> Float
// CHECK: [[ORIG_FN:%.*]] = thin_to_thick_function [[ORIG_FN_THIN]]
// CHECK: [[LINEAR_FN_ESCAPING:%.*]] = linear_function [parameters 0 1] [[ORIG_FN]]
// CHECK: [[LINEAR_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[LINEAR_FN_ESCAPING]]
// CHECK: [[TRANSPOSE_FN:%.*]] = linear_function_extract [transpose] [[LINEAR_FN]]
// CHECK: [[RESULT:%.*]] = apply [[TRANSPOSE_FN]]([[X]]) : $@noescape @callee_guaranteed (Float) -> (Float, Float)
// CHECK: ([[RESULT_0:%.*]], [[RESULT_1:%.*]]) = destructure_tuple [[RESULT]]
// CHECK: destroy_value [[LINEAR_FN_ESCAPING]]
// CHECK: [[RETUPLED_RESULT:%.*]] = tuple ([[RESULT_0]] : $Float, [[RESULT_1]] : $Float)
// CHECK: return [[RETUPLED_RESULT]]

@_silgen_name("applyTranspose_f_indirect_arity1")
func applyTranspose_f_indirect_arity1<T: AdditiveArithmetic & Differentiable>(_ x: T) -> T {
  return Builtin.applyTranspose_arity1(f_indirect_arity1, x)
}
// CHECK-LABEL: sil{{.*}}@applyTranspose_f_indirect_arity1
// CHECK: bb0([[OUT_PARAM:%.*]] : $*T, [[X:%.*]] : $*T):
// CHECK: [[RESULT:%.*]] = apply [[TRANSPOSE:%.*]]([[OUT_PARAM]], [[X]])

// MARK: - differentiableFunction

@_silgen_name("differentiableFunction_f_direct_arity1")
func differentiableFunction_f_direct_arity1() -> @differentiable (Float) -> Float {
  return Builtin.differentiableFunction_arity1(f_direct_arity1, f_direct_arity1_jvp, f_direct_arity1_vjp)
}
// CHECK-LABEL: sil{{.*}}@differentiableFunction_f_direct_arity1
// CHECK: [[DIFF_FN:%.*]] = differentiable_function
// CHECK: return [[DIFF_FN]]

// MARK: - linearFunction

@_silgen_name("linearFunction_f_direct_arity1")
func linearFunction_f_direct_arity1() -> @differentiable(linear) (Float) -> Float {
  return Builtin.linearFunction_arity1(f_direct_arity1, f_direct_arity1)
}
// CHECK-LABEL: sil{{.*}}@linearFunction_f_direct_arity1
// CHECK: [[LINEAR_FN:%.*]] = linear_function
// CHECK: return [[LINEAR_FN]]
