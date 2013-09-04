// RUN: %swift -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil @_T13auto_closures17call_auto_closureFT1xFT_Sb_Sb
func call_auto_closure(x : [auto_closure] () -> Bool) -> Bool {
  // CHECK: [[XBOX:%.*]] = alloc_box $() -> Bool
  // CHECK: [[XLOAD:%.*]] = load [[XBOX]]#1
  // CHECK: [[RET:%.*]] = apply [transparent] [[XLOAD]]()
  // CHECK: return [[RET]]
  return x()
}

// CHECK-LABEL sil @_T13auto_closures30test_auto_closure_with_captureFT1xSb_Sb
func test_auto_closure_with_capture(x : Bool) -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @closure{{[0-9]+}}
  // CHECK: [[WITHCAPTURE:%.*]] = partial_apply [[CLOSURE]](
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[WITHCAPTURE]])
  // CHECK: return [[RET]]
  return call_auto_closure(x)
}

// CHECK-LABEL: sil @_T13auto_closures33test_auto_closure_without_captureFT_Sb
func test_auto_closure_without_capture() -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @closure{{[0-9]+}} : $[thin] () -> Bool
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $[thin] () -> Bool to $() -> Bool
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[THICK]])
  // CHECK: return [[RET]]
  return call_auto_closure(false)
}
