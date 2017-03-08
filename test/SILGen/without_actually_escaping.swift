// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden @_T025without_actually_escaping9letEscapeyycyyc1f_tF
func letEscape(f: () -> ()) -> () -> () {
  // CHECK: bb0([[ARG:%.*]] : $@callee_owned () -> ()):
  // TODO: Use a canary wrapper instead of just copying the nonescaping value
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: [[ESCAPABLE_COPY:%.*]] = copy_value [[BORROWED_ARG]]
  // CHECK: [[SUB_CLOSURE:%.*]] = function_ref @
  // CHECK: [[RESULT:%.*]] = apply [[SUB_CLOSURE]]([[ESCAPABLE_COPY]])
  // CHECK: end_borrow [[BORROWED_ARG]] from [[ARG]]
  // CHECK: destroy_value [[ARG]]
  // CHECK: return [[RESULT]]
  return withoutActuallyEscaping(f) { return $0 }
}
