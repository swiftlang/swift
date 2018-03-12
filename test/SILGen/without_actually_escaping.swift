// REQUIRES: plus_one_runtime

// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden @$S25without_actually_escaping9letEscape1fyycyyXE_tF
func letEscape(f: () -> ()) -> () -> () {
// CHECK: bb0([[ARG:%.*]] : @trivial $@noescape @callee_guaranteed () -> ()):
// CHECK:  [[CVT:%.*]] = function_ref @$SIg_Ieg_TR : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
// TODO: verify that the partial_apply's reference count is one at the end of the scope.
// CHECK:  [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CVT]]([[ARG]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
// CHECK:  [[MD:%.*]] = mark_dependence  [[CLOSURE]] : $@callee_guaranteed () -> () on [[ARG]] : $@noescape @callee_guaranteed () -> ()
// CHECK:  [[BORROW:%.*]] = begin_borrow [[MD]] : $@callee_guaranteed () -> ()
// CHECK:  [[COPY:%.*]] = copy_value [[BORROW]] : $@callee_guaranteed () -> ()
// CHECK:  [[USER:%.*]] = function_ref @$S25without_actually_escaping9letEscape1fyycyyXE_tFyycyycXEfU_ : $@convention(thin) (@owned @callee_guaranteed () -> ()) -> @owned @callee_guaranteed () -> ()
// CHECK:  [[RES:%.*]] = apply [[USER]]([[COPY]]) : $@convention(thin) (@owned @callee_guaranteed () -> ()) -> @owned @callee_guaranteed () -> ()
// CHECK:  [[ESCAPED:%.*]] = is_escaping_closure [[BORROW]] : $@callee_guaranteed () -> ()
// CHECK:  cond_fail [[ESCAPED]] : $Builtin.Int1
// CHECK:  end_borrow [[BORROW]] from [[MD]] : $@callee_guaranteed () -> (), $@callee_guaranteed () -> ()
// CHECK:  destroy_value [[MD]]
// CHECK:  return [[RES]] : $@callee_guaranteed () -> ()
  return withoutActuallyEscaping(f) { return $0 }
}


// CHECK-LABEL: sil hidden @$S25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKF
// CHECK: bb0([[ARG:%.*]] : @trivial $@noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error Error)):
// CHECK: [[CVT:%.*]] = function_ref @$SIeg_s5Error_pIgozo_Ieg_sAA_pIegozo_TR
// CHECK: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CVT]]([[ARG]])
// CHECK:  [[MD:%.*]] = mark_dependence [[CLOSURE]] : {{.*}} on [[ARG]]
// CHECK:  [[BORROW:%.*]] = begin_borrow [[MD]]
// CHECK:  [[COPY:%.*]] = copy_value [[BORROW]]
// CHECK:  [[USER:%.*]] = function_ref @$S25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKFyycyycyKcKXEfU_
// CHECK:  try_apply [[USER]]([[COPY]]) : {{.*}}, normal bb1, error bb2
//
// CHECK: bb1([[RES:%.*]] : @owned $@callee_guaranteed () -> ()):
// CHECK:   [[ESCAPED:%.*]] = is_escaping_closure [[BORROW]]
// CHECK:   cond_fail [[ESCAPED]] : $Builtin.Int1
// CHECK:   end_borrow [[BORROW]] from [[MD]]
// CHECK:   destroy_value [[MD]]
// CHECK:   return [[RES]]
//
// CHECK: bb2([[ERR:%.*]] : @owned $Error):
// CHECK:   end_borrow [[BORROW]] from [[MD]]
// CHECK:   destroy_value [[MD]]
// CHECK:   throw [[ERR]] : $Error
// CHECK: }

func letEscapeThrow(f: () throws -> () -> ()) throws -> () -> () {
  return try withoutActuallyEscaping(f) { return try $0() }
}
