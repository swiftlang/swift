// RUN: %target-swift-frontend -parse-stdlib -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck -check-prefix=CHECK-SIL %s

import Swift

func evaldiff<T: Differentiable, U: Differentiable>(_ f: @differentiable (T) -> U, _ x: T) -> (U, (T.TangentVector) -> U.TangentVector)
  where T == T.TangentVector {
  return Builtin.applyDerivative_jvp(f, x)
}

// CHECK-SIL-LABEL: @{{.*}}evaldiff{{.*}}
// CHECK-SIL: bb0([[ORIG_RES_BUF:%.*]] : $*U, [[ORIG_FN:%.*]] : $@differentiable @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U>, [[ORIG_FN_ARG:%.*]] : $*T):
// CHECK-SIL:   [[ORIG_FN_CONVERTED:%.*]] = convert_function [[ORIG_FN]] : $@differentiable @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U> to $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[JVP_FN:%.*]] = differentiable_function_extract [jvp] [[ORIG_FN_CONVERTED]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[JVP_RES_BUF:%.*]] = alloc_stack $(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>)
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>), 0
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = apply [[JVP_FN]]([[JVP_RES_BUF_0]], [[ORIG_FN_ARG]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>)
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>), 1
// CHECK-SIL:   store [[DIFFERENTIAL]] to [init] [[JVP_RES_BUF_1]] : $*@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>), 0
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>), 1
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = load [take] [[JVP_RES_BUF_1]] : $*@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>
// CHECK-SIL:   copy_addr [take] [[JVP_RES_BUF_0]] to [initialization] [[ORIG_RES_BUF]] : $*U
// CHECK-SIL:   dealloc_stack [[JVP_RES_BUF]] : $*(U, @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>)
// CHECK-SIL:   return [[DIFFERENTIAL]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <T, U.TangentVector>

func evaldiff2<T: Differentiable, U: Differentiable, V: Differentiable>(_ f: @differentiable (T, U) -> V, _ x: T, _ y: U) -> (V, (T.TangentVector, U.TangentVector) -> V.TangentVector)
  where T == T.TangentVector, U == U.TangentVector {
  return Builtin.applyDerivative_jvp_arity2(f, x, y)
}

// CHECK-LABEL: @{{.*}}evaldiff2{{.*}}
// CHECK: bb0({{.*}} : $*V, [[DIFFED:%.*]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out V, {{.*}} : $*T, {{.*}} : $*U):
// CHECK:   differentiable_function_extract [jvp] [[DIFFED]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out V // user: %14
