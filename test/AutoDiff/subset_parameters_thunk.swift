// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@differentiable(where T: Differentiable)
@_silgen_name("foo")
func foo<T: Numeric>(_ x: T, _ y: T) -> T { x * y }

@differentiating(foo)
@_silgen_name("foo_vjp")
func foo_vjp<T: Numeric & Differentiable>(_ x: T, _ y: T) -> (value: T, pullback: (T.TangentVector) -> (T.TangentVector, T.TangentVector)) {
  (foo(x, y), { _ in (.zero, .zero) })
}

let x = Float(1)
@differentiable
func differentiate_foo_wrt_0(_ x: Float) -> Float {
  foo(x, 1)
}

// CHECK-LABEL: // VJP wrt 0 source 0 for differentiate_foo_wrt_0(_:)
// CHECK-LABEL: sil hidden @$s23subset_parameters_thunk23differentiate_foo_wrt_0yS2fFTZp0r0
// CHECK: bb0
// CHECK:   [[FOO_ORIG:%.*]] = function_ref @foo : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[FOO_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_ORIG]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   [[FOO_JVP:%.*]] = function_ref @$s3fooTzp0_1r0 : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector, @in_guaranteed τ_0_0.TangentVector) -> @out τ_0_0.TangentVector)
// CHECK:   [[FOO_JVP_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_JVP]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector, @in_guaranteed τ_0_0.TangentVector) -> @out τ_0_0.TangentVector)
// CHECK:   release_value [[FOO_JVP_FLOAT]]
// CHECK:   [[FOO_JVP_SUBSET_THUNK_THIN:%.*]] = function_ref @AD__orig_{{.*}}foo{{.*}}_src_0_wrt_0_jvp_subset_parameters_thunk : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_JVP_SUBSET_THUNK:%.*]] = thin_to_thick_function [[FOO_JVP_SUBSET_THUNK_THIN]] : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) to $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_VJP:%.*]] = function_ref @$s3fooTZp0_1r0 : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector) -> (@out τ_0_0.TangentVector, @out τ_0_0.TangentVector))
// CHECK:   [[FOO_VJP_FLOAT:%.*]] = partial_apply [callee_guaranteed] [[FOO_VJP]]<Float>() : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 : Numeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> (@out τ_0_0, @owned @callee_guaranteed (@in_guaranteed τ_0_0.TangentVector) -> (@out τ_0_0.TangentVector, @out τ_0_0.TangentVector))
// CHECK:   release_value [[FOO_VJP_FLOAT]]
// CHECK:   [[FOO_VJP_SUBSET_THUNK_THIN:%.*]] = function_ref @AD__orig_{{.*}}foo{{.*}}_src_0_wrt_0_vjp_subset_parameters_thunk : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_VJP_SUBSET_THUNK:%.*]] = thin_to_thick_function [[FOO_VJP_SUBSET_THUNK_THIN]] : $@convention(thin) (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) to $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
// CHECK:   [[FOO_DIFF:%.*]] = autodiff_function [wrt 0] [order 1] [[FOO_FLOAT]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> @out Float with {[[FOO_JVP_SUBSET_THUNK]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float), [[FOO_VJP_SUBSET_THUNK]] : $@callee_guaranteed (@in_guaranteed Float, @in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)}
// CHECK: }
