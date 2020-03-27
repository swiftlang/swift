// RUN: %target-swift-frontend -parse-stdlib -emit-silgen -enable-experimental-differentiable-programming %s | %FileCheck %s

import _Differentiation
import Swift

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
// TODO(TF-1142): Add linear_function_extracts to this test when they exist.

@_silgen_name("applyTranspose_f_direct_arity1")
func applyTranspose_f_direct_arity1(_ x: Float) -> Float {
  return Builtin.applyTranspose_arity1(f_direct_arity1, x)
}
// CHECK-LABEL: sil{{.*}}@applyTranspose_f_direct_arity1
// CHECK: bb0([[X:%.*]] : $Float):
// CHECK: [[RESULT:%.*]] = apply undef([[X]])
// CHECK: return [[RESULT]]

@_silgen_name("applyTranspose_f_direct_arity2")
func applyTranspose_f_direct_arity2(_ x: Float) -> (Float, Float) {
  return Builtin.applyTranspose_arity2(f_direct_arity2, x)
}
// CHECK-LABEL: sil{{.*}}@applyTranspose_f_direct_arity2
// CHECK: bb0([[X:%.*]] : $Float)
// CHECK: [[RESULT:%.*]] = apply undef([[X]])
// CHECK: ([[RESULT_0:%.*]], [[RESULT_1:%.*]]) = destructure_tuple [[RESULT]]
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
// TODO(TF-1142): Add linear_funcion to this test when it exists.

@_silgen_name("linearFunction_f_direct_arity1")
func linearFunction_f_direct_arity1() -> @differentiable(linear) (Float) -> Float {
  return Builtin.linearFunction_arity1(f_direct_arity1, f_direct_arity1)
}
// CHECK-LABEL: sil{{.*}}@linearFunction_f_direct_arity1
// CHECK: return undef
