// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden @_T025without_actually_escaping9letEscapeyycyyc1f_tF
func letEscape(f: () -> ()) -> () -> () {
  // TODO: Use a canary wrapper instead of just copying the nonescaping value
  // CHECK: [[ESCAPABLE_COPY:%.*]] = copy_value %0
  // CHECK: [[SUB_CLOSURE:%.*]] = function_ref @
  // CHECK: [[RESULT:%.*]] = apply [[SUB_CLOSURE]]([[ESCAPABLE_COPY]])
  // CHECK: destroy_value %0
  // CHECK: return [[RESULT]]
  return withoutActuallyEscaping(f) { return $0 }
}
