// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name without_actually_escaping %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden [ossa] @$s25without_actually_escaping9letEscape1fyycyyXE_tF
func letEscape(f: () -> ()) -> () -> () {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> ()):
  // CHECK: [[ARGA:%.*]] = copy_value [[ARG]]
  // CHECK: [[ARGB:%.*]] = copy_value [[ARGA]]
  // CHECK: [[THUNK:%.*]] = function_ref @$sIg_Ieg_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> ()) -> ()
  // TODO: Use a canary wrapper instead of just copying the nonescaping value
  // CHECK: [[ESCAPABLE_COPY:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[ARGB]])
  // CHECK: [[MD_ESCAPABLE_COPY:%.*]] = mark_dependence [[ESCAPABLE_COPY]]
  // CHECK: [[BORROW_MD_ESCAPABLE_COPY:%.*]] = begin_borrow [[MD_ESCAPABLE_COPY]]
  // CHECK: [[SUB_CLOSURE:%.*]] = function_ref @
  // CHECK: [[RESULT:%.*]] = apply [[SUB_CLOSURE]]([[BORROW_MD_ESCAPABLE_COPY]])
  // CHECK: destroy_not_escaped_closure [[MD_ESCAPABLE_COPY]]
  // CHECK: return [[RESULT]]
  return withoutActuallyEscaping(f) { return $0 }
}

// thunk for @callee_guaranteed () -> ()
// The thunk must be [without_actually_escaping].
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [without_actually_escaping] [ossa] @$sIg_Ieg_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> ()) -> () {

// CHECK-LABEL: sil hidden [ossa] @$s25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error any Error)):
// CHECK: [[ARGA:%.*]] = copy_value [[ARG]]
// CHECK: [[ARGB:%.*]] = copy_value [[ARGA]]
// CHECK: [[CVT:%.*]] = function_ref @$sIeg_s5Error_pIgozo_Ieg_sAA_pIegozo_TR
// CHECK: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[CVT]]([[ARGB]])
// CHECK:  [[MD:%.*]] = mark_dependence [[CLOSURE]] : {{.*}} on [[ARGA]]
// CHECK:  [[BORROW:%.*]] = begin_borrow [[MD]]
// CHECK:  [[USER:%.*]] = function_ref @$s25without_actually_escaping14letEscapeThrow1fyycyycyKXE_tKFyycyycyKcKXEfU_
// CHECK:  try_apply [[USER]]([[BORROW]]) : {{.*}}, normal bb1, error bb2
//
// CHECK: bb1([[RES:%.*]] : @owned $@callee_guaranteed () -> ()):
// CHECK:   end_borrow [[BORROW]]
// CHECK:   [[ESCAPED:%.*]] = destroy_not_escaped_closure [[MD]]
// CHECK:   cond_fail [[ESCAPED]] : $Builtin.Int1
// CHECK:   return [[RES]]
//
// CHECK: bb2([[ERR:%.*]] : @owned $any Error):
// CHECK:   end_borrow [[BORROW]]
// CHECK:   [[ESCAPED:%.*]] = destroy_not_escaped_closure [[MD]]
// CHECK:   cond_fail [[ESCAPED]] : $Builtin.Int1
// CHECK:   throw [[ERR]] : $any Error
// CHECK: }

func letEscapeThrow(f: () throws -> () -> ()) throws -> () -> () {
  return try withoutActuallyEscaping(f) { return try $0() }
}

enum HomeworkError: Error {
case forgot
case dogAteIt
}

// CHECK-LABEL: sil hidden [ossa] @$s25without_actually_escaping19letEscapeThrowTyped1fyycyycyAA13HomeworkErrorOYKXE_tKF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error HomeworkError)) -> (@owned @callee_guaranteed () -> (), @error any Error)
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error HomeworkError)):
// CHECK: function_ref @$sIeg_25without_actually_escaping13HomeworkErrorOIgozo_Ieg_ACIegozo_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error HomeworkError)) -> (@owned @callee_guaranteed () -> (), @error HomeworkError)
// CHECK: bb2([[ERR:%.*]] : @owned $any Error):
// CHECK:   end_borrow [[BORROW]]
// CHECK:   destroy_not_escaped_closure [[MD]]
// CHECK:   throw [[ERR]] : $any Error
// CHECK: }

func letEscapeThrowTyped(f: () throws(HomeworkError) -> () -> ()) throws -> () -> () {
  return try withoutActuallyEscaping(f) { return try $0() }
}

// The thunk must be [without_actually_escaping].
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [without_actually_escaping] [ossa] @$sIeg_25without_actually_escaping13HomeworkErrorOIgozo_Ieg_ACIegozo_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> (@owned @callee_guaranteed () -> (), @error HomeworkError)) -> (@owned @callee_guaranteed () -> (), @error HomeworkError)

func letEscapeThrowTyped2(f: () throws(HomeworkError) -> () -> ()) throws(HomeworkError) -> () -> () {
  return try withoutActuallyEscaping(f) { (g) throws(HomeworkError) in return try g() }
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

func modifyAndPerform<T>(_ _: UnsafeMutablePointer<T>, closure: () ->()) {
  closure()
}

// Make sure that we properly handle cases where the input closure is not
// trivial. This means we need to copy first.
// CHECK-LABEL: sil hidden [ossa] @$s25without_actually_escaping0A24ActuallyEscapingConflictyyF : $@convention(thin) () -> () {
// CHECK: [[CLOSURE_1_FUN:%.*]] = function_ref @$s25without_actually_escaping0A24ActuallyEscapingConflictyyFyycfU_ :
// CHECK: [[CLOSURE_1:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_1_FUN]](
// CHECK: [[MOVED_CLOSURE_1:%.*]] = move_value [lexical] [var_decl] [[CLOSURE_1]]
// CHECK: [[BORROWED_CLOSURE_1:%.*]] = begin_borrow [[MOVED_CLOSURE_1]]
// CHECK: [[COPY_BORROWED_CLOSURE_1:%.*]] = copy_value [[BORROWED_CLOSURE_1]]
// CHECK: [[COPY_2_BORROWED_CLOSURE_1:%.*]] = copy_value [[COPY_BORROWED_CLOSURE_1]]
// CHECK: [[THUNK_FUNC:%.*]] = function_ref @$sIeg_Ieg_TR :
// CHECK: [[THUNK_PA:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FUNC]]([[COPY_2_BORROWED_CLOSURE_1]])
// CHECK: [[THUNK_PA_MDI:%.*]] = mark_dependence [[THUNK_PA]] : $@callee_guaranteed () -> () on [[COPY_BORROWED_CLOSURE_1]] : $@callee_guaranteed () -> ()
// CHECK: destroy_not_escaped_closure [[THUNK_PA_MDI]]
// CHECK: destroy_value [[COPY_BORROWED_CLOSURE_1]]
// CHECK: } // end sil function '$s25without_actually_escaping0A24ActuallyEscapingConflictyyF'
func withoutActuallyEscapingConflict() {
  var localVar = 0
  let nestedModify = { localVar = 3 }
  withoutActuallyEscaping(nestedModify) {
    modifyAndPerform(&localVar, closure: $0)
  }
}

// CHECK-LABEL: sil [ossa] @$s25without_actually_escaping0A25ActuallyEscapingCFunction8functionyyyXC_tF
// CHECK: bb0([[ARG:%.*]] : $@convention(c) () -> ()):
// CHECK:   [[E:%.*]] = convert_function [[ARG]] : $@convention(c) () -> () to [without_actually_escaping] $@convention(c) () -> ()
// CHECK:   [[F:%.*]] = function_ref @$s25without_actually_escaping0A25ActuallyEscapingCFunction8functionyyyXC_tFyyyXCXEfU_ : $@convention(thin) (@convention(c) () -> ()) -> ()
// CHECK:   apply [[F]]([[E]]) : $@convention(thin) (@convention(c) () -> ()) -> ()
public func withoutActuallyEscapingCFunction(function: (@convention(c) () -> Void)) {
  withoutActuallyEscaping(function) { f in
    var pointer: UnsafeRawPointer? = nil
    pointer = unsafeBitCast(f, to: UnsafeRawPointer.self)
    print(pointer)
  }
}
