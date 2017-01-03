// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

var escapeHatch: Any = 0

// CHECK-LABEL: sil hidden @_TF25without_actually_escaping9letEscapeFT1fFT_T__FT_T_
func letEscape(f: () -> ()) -> () -> () {
  // TODO: Use a canary wrapper instead of just copying the nonescaping value
  // CHECK: [[ESCAPABLE_COPY:%.*]] = copy_value %0
  // CHECK: [[SUB_CLOSURE:%.*]] = function_ref @
  // CHECK: [[RESULT:%.*]] = apply [[SUB_CLOSURE]]([[ESCAPABLE_COPY]])
  // CHECK: destroy_value %0
  // CHECK: return [[RESULT]]
  return withoutActuallyEscaping(f) { return $0 }
}
