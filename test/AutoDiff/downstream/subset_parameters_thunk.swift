// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

func foo<T: Numeric>(_ x: T, _ y: T) -> T { x * y }

@derivative(of: foo)
func foo_vjp<T: Numeric & Differentiable>(_ x: T, _ y: T) -> (value: T, pullback: (T.TangentVector) -> (T.TangentVector, T.TangentVector)) {
  (foo(x, y), { _ in (.zero, .zero) })
}

let x = Float(1)
@differentiable
func differentiate_foo_wrt_0(_ x: Float) -> Float {
  foo(x, 1)
}

// Intentional "//" in the label so that this doesn't match a [differentiable] attr pointing at the vjp.
// CHECK-LABEL: // {{.*}}differentiate_foo_wrt_0{{.*}}__vjp
// CHECK: bb0
// CHECK:   [[FOO_ORIG:%.*]] = function_ref @{{.*}}foo{{.*}} : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[FOO_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_ORIG]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[FOO_JVP:%.*]] = differentiability_witness_function [jvp] [parameters 0 1] [results 0] <T where T : Differentiable, T : Numeric> @{{.*}}foo{{.*}} : $@convention(thin) <T where T : Numeric> (@in_guaranteed T, @in_guaranteed T) -> @out T
// CHECK:   [[FOO_JVP_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_JVP]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_2 for <τ_0_0.TangentVector, τ_0_0.TangentVector, τ_0_0.TangentVector>)
// CHECK:   [[FOO_JVP_SUBSET_THUNK_THIN:%.*]] = function_ref @AD__orig_{{.*}}foo{{.*}}_src_0_wrt_0_jvp_subset_parameters_thunk : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_JVP_SUBSET_THUNK:%.*]] = thin_to_thick_function [[FOO_JVP_SUBSET_THUNK_THIN]] : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) to $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_VJP:%.*]] = differentiability_witness_function [vjp] [parameters 0 1] [results 0] <T where T : Differentiable, T : Numeric> @{{.*}}foo{{.*}} : $@convention(thin) <T where T : Numeric> (@in_guaranteed T, @in_guaranteed T) -> @out T
// CHECK:   [[FOO_VJP_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_VJP]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0) -> (@out τ_0_1, @out τ_0_2) for <τ_0_0.TangentVector, τ_0_0.TangentVector, τ_0_0.TangentVector>)
// CHECK:   [[FOO_VJP_SUBSET_THUNK_THIN:%.*]] = function_ref @AD__orig_{{.*}}foo{{.*}}_src_0_wrt_0_vjp_subset_parameters_thunk : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_VJP_SUBSET_THUNK:%.*]] = thin_to_thick_function [[FOO_VJP_SUBSET_THUNK_THIN]] : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) to $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_DIFF:%.*]] = differentiable_function [parameters 0] [[FOO_FLOAT]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float with_derivative {[[FOO_JVP_SUBSET_THUNK]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float), [[FOO_VJP_SUBSET_THUNK]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)}
// CHECK: }
