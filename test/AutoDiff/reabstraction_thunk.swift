// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// Test reabstraction thunk differentiation.
// Check that reabstraction thunk JVP/VJP functions have a `@differentiable`
// function-typed argument.

@_silgen_name("id")
func id<T>(_ x: T) -> T { x }
let _: @differentiable (Float) -> Float = id

// CHECK-LABEL: sil_differentiability_witness shared [serialized] [parameters 0] [results 0] @$sS2fIegnr_S2fIegyd_TR : $@convention(thin) (Float, @guaranteed @callee_guaranteed (@in_guaranteed Float) -> @out Float) -> Float {
// CHECK:   jvp: @AD__$sS2fIegnr_S2fIegyd_TR__jvp_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed Float) -> @out Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK:   vjp: @AD__$sS2fIegnr_S2fIegyd_TR__vjp_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed Float) -> @out Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK: }

// CHECK-LABEL: sil hidden [serializable] @AD__$sS2fIegnr_S2fIegyd_TR__vjp_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed @differentiable @callee_guaranteed (@in_guaranteed Float) -> @out Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
