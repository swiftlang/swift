
// RUN: %target-swift-emit-silgen -module-name without_actually_escaping -enable-sil-ownership %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden @$S25without_actually_escaping9letEscape1fyycyyXE_tF
func letEscape(f: () -> ()) -> () -> () {
  // CHECK: bb0([[ARG:%.*]] : @trivial $@noescape @callee_guaranteed () -> ()):
  // TODO: Use a canary wrapper instead of just copying the nonescaping value
  // CHECK: [[ESCAPABLE_COPY:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[ARG]])
  // CHECK: [[MD_ESCAPABLE_COPY:%.*]] = mark_dependence [[ESCAPABLE_COPY]]
  // CHECK: [[BORROW_MD_ESCAPABLE_COPY:%.*]] = begin_borrow [[MD_ESCAPABLE_COPY]]
  // CHECK: [[SUB_CLOSURE:%.*]] = function_ref @
  // CHECK: [[RESULT:%.*]] = apply [[SUB_CLOSURE]]([[BORROW_MD_ESCAPABLE_COPY]])
  // CHECK: destroy_value [[MD_ESCAPABLE_COPY]]
  // CHECK: return [[RESULT]]
  return withoutActuallyEscaping(f) { return $0 }
}


// CHECK-LABEL: sil hidden @$S25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKF
// CHECK: bb0([[ARG:%.*]] : @trivial $@noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error Error)):
// CHECK: [[CVT:%.*]] = function_ref @$SIeg_s5Error_pIgozo_Ieg_sAA_pIegozo_TR
// CHECK: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CVT]]([[ARG]])
// CHECK:  [[MD:%.*]] = mark_dependence [[CLOSURE]] : {{.*}} on [[ARG]]
// CHECK:  [[BORROW:%.*]] = begin_borrow [[MD]]
// CHECK:  [[USER:%.*]] = function_ref @$S25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKFyycyycyKcKXEfU_
// CHECK:  try_apply [[USER]]([[BORROW]]) : {{.*}}, normal bb1, error bb2
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

// We used to crash on this example because we would use the wrong substitution
// map.
struct DontCrash {
  private func firstEnv<L1>(
    closure1: (L1) -> Bool,
    closure2: (L1) -> Bool
  ) {
    withoutActuallyEscaping(closure1) { closure1 in
        secondEnv(
            closure1: closure1,
            closure2: closure2
        )
    }
  }

  private func secondEnv<L2>(
    closure1: @escaping (L2) -> Bool,
    closure2: (L2) -> Bool
  ) {
    withoutActuallyEscaping(closure2) { closure2 in
    }
  }
}
