// RUN: %target-swift-frontend -parse-stdlib -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck -check-prefix=CHECK-SIL %s

import Swift

func evaldiff<T: Differentiable, U: Differentiable>(_ f: @differentiable (T) -> U, _ x: T) -> (U, (T.TangentVector) -> U.TangentVector)
  where T == T.TangentVector {
  return Builtin.autodiffApply_jvp(f, x)
}

// CHECK-SIL-LABEL: @{{.*}}evaldiff{{.*}}
// CHECK-SIL: bb0([[ORIG_RES_BUF:%.*]] : @trivial $*U, [[ORIG_FN:%.*]] : @trivial $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @out U, [[ORIG_FN_ARG:%.*]] : @trivial $*T):
// CHECK-SIL:   [[ORIG_FN_ARG_COPY:%.*]] = alloc_stack $T
// CHECK-SIL:   copy_addr [[ORIG_FN_ARG]] to [initialization] [[ORIG_FN_ARG_COPY]] : $*T
// CHECK-SIL:   [[JVP_FN:%.*]] = autodiff_function_extract [jvp] [order 1] [[ORIG_FN]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[JVP_RES_BUF:%.*]] = alloc_stack $(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector)
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector), 0
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = apply [[JVP_FN]]([[JVP_RES_BUF_0]], [[ORIG_FN_ARG_COPY]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector)
// CHECK-SIL:   destroy_addr [[ORIG_FN_ARG_COPY]] : $*T
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector), 1
// CHECK-SIL:   store [[DIFFERENTIAL]] to [init] [[JVP_RES_BUF_1]] : $*@callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector), 0
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector), 1
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = load [take] [[JVP_RES_BUF_1]] : $*@callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector
// CHECK-SIL:   copy_addr [take] [[JVP_RES_BUF_0]] to [initialization] [[ORIG_RES_BUF]] : $*U
// CHECK-SIL:   dealloc_stack [[JVP_RES_BUF]] : $*(U, @callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector)
// CHECK-SIL:   dealloc_stack [[ORIG_FN_ARG_COPY]] : $*T
// CHECK-SIL:   return [[DIFFERENTIAL]] : $@callee_guaranteed (@in_guaranteed T) -> @out U.TangentVector

func evaldiff2<T: Differentiable, U: Differentiable, V: Differentiable>(_ f: @differentiable (T, U) -> V, _ x: T, _ y: U) -> (V, (T.TangentVector, U.TangentVector) -> V.TangentVector)
  where T == T.TangentVector, U == U.TangentVector {
  return Builtin.autodiffApply_jvp_arity2(f, x, y)
}

// CHECK-LABEL: @{{.*}}evaldiff2{{.*}}
// CHECK: bb0({{.*}} : @trivial $*V, [[DIFFED:%.*]] : @trivial $@differentiable @noescape @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out V, {{.*}} : @trivial $*T, {{.*}} : @trivial $*U):
// CHECK:   autodiff_function_extract [jvp] [order 1] [[DIFFED]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out V // user: %14
