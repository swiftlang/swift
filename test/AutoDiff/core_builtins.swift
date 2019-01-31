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

func methoddiff<T: Differentiable, U: Differentiable, R: Differentiable>(_ f: @differentiable (T) -> (U) -> R, _ x: T, _ y: U)
    -> (R, (T.TangentVector, U.TangentVector) -> R.TangentVector)
  where T == T.TangentVector, U == U.TangentVector {
  return Builtin.autodiffApply_jvp_method(f, x, y)
}

// CHECK-SIL-LABEL: @{{.*}}methoddiff{{.*}}
// CHECK-SIL: bb0([[ORIG_RES_BUF:%.*]] : @trivial $*R, [[ORIG_FN:%.*]] : @trivial $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @owned @callee_guaranteed (@in_guaranteed U) -> @out R, [[ORIG_FN_ARG1:%.*]] : @trivial $*T, [[ORIG_FN_ARG2:%.*]] : @trivial $*U):
// CHECK-SIL:   [[ORIG_FN_ARG1_COPY:%.*]] = alloc_stack $T
// CHECK-SIL:   copy_addr [[ORIG_FN_ARG1]] to [initialization] [[ORIG_FN_ARG1_COPY]] : $*T
// CHECK-SIL:   [[ORIG_FN_ARG2_COPY:%.*]] = alloc_stack $U
// CHECK-SIL:   copy_addr [[ORIG_FN_ARG2]] to [initialization] [[ORIG_FN_ARG2_COPY]] : $*U
// CHECK-SIL:   [[JVP_FN:%.*]] = autodiff_function_extract [jvp] [order 1] [[ORIG_FN]] : $@differentiable @noescape @callee_guaranteed (@in_guaranteed T) -> @owned @callee_guaranteed (@in_guaranteed U) -> @out R
// CHECK-SIL:   [[JVP_FN_PARTIAL_APPLIED:%.*]] = apply [[JVP_FN]]([[ORIG_FN_ARG1_COPY]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> @owned @callee_guaranteed (@in_guaranteed U) -> (@out R, @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector)
// CHECK-SIL:   destroy_addr [[ORIG_FN_ARG1_COPY]]
// CHECK-SIL:   [[JVP_RES_BUF:%.*]] = alloc_stack $(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector)
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector), 0
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = apply [[JVP_FN_PARTIAL_APPLIED]]([[JVP_RES_BUF_0]], [[ORIG_FN_ARG2_COPY]]) : $@callee_guaranteed (@in_guaranteed U) -> (@out R, @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector)
// CHECK-SIL:   destroy_value [[JVP_FN_PARTIAL_APPLIED]]
// CHECK-SIL:   destroy_addr [[ORIG_FN_ARG2_COPY]] : $*U
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector), 1
// CHECK-SIL:   store [[DIFFERENTIAL]] to [init] [[JVP_RES_BUF_1]] : $*@callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector
// CHECK-SIL:   [[JVP_RES_BUF_0:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector), 0
// CHECK-SIL:   [[JVP_RES_BUF_1:%.*]] = tuple_element_addr [[JVP_RES_BUF]] : $*(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector), 1
// CHECK-SIL:   [[DIFFERENTIAL:%.*]] = load [take] [[JVP_RES_BUF_1]] : $*@callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector
// CHECK-SIL:   copy_addr [take] [[JVP_RES_BUF_0]] to [initialization] [[ORIG_RES_BUF]] : $*R
// CHECK-SIL:   dealloc_stack [[JVP_RES_BUF]] : $*(R, @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector)
// CHECK-SIL:   dealloc_stack [[ORIG_FN_ARG2_COPY]] : $*U
// CHECK-SIL:   dealloc_stack [[ORIG_FN_ARG1_COPY]] : $*T
// CHECK-SIL:   return [[DIFFERENTIAL]] : $@callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out R.TangentVector

