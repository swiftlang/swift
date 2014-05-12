// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s

struct Bool {}
var false = Bool()

// CHECK-LABEL: sil @_TF13auto_closures17call_auto_closure
func call_auto_closure(var x: @auto_closure () -> Bool) -> Bool {
  // CHECK: [[XBOX:%.*]] = alloc_box $@callee_owned () -> Bool
  // CHECK: [[XLOAD:%.*]] = load [[XBOX]]#1
  // CHECK: [[RET:%.*]] = apply [transparent] [[XLOAD]]()
  // CHECK: return [[RET]]
  return x()
}

// CHECK-LABEL: sil @_TF13auto_closures18call_auto_closure2
func call_auto_closure2(inout x: @auto_closure () -> Bool) -> Bool {
  // CHECK: [[XBOX:%.*]] = alloc_box $@callee_owned () -> Bool
  // CHECK: [[XLOAD:%.*]] = load [[XBOX]]#1
  // CHECK: [[RET:%.*]] = apply [transparent] [[XLOAD]]()
  // CHECK: return [[RET]]
  return x()
}

// CHECK-LABEL sil @_TF13auto_closures30test_auto_closure_with_capture
func test_auto_closure_with_capture(x: Bool) -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @_TFF13auto_closures30test_auto_closure_with_capture
  // CHECK: [[WITHCAPTURE:%.*]] = partial_apply [[CLOSURE]](
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[WITHCAPTURE]])
  // CHECK: return [[RET]]
  return call_auto_closure(x)
}

// CHECK-LABEL: sil @_TF13auto_closures33test_auto_closure_without_capture
func test_auto_closure_without_capture() -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @_TFF13auto_closures33test_auto_closure_without_capture
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@thin () -> Bool to $@callee_owned () -> Bool
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[THICK]])
  // CHECK: return [[RET]]
  return call_auto_closure(false)
}
