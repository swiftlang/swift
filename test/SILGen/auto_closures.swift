
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name auto_closures -parse-stdlib -swift-version 5 %s | %FileCheck %s

struct Bool {}
var false_ = Bool()

// CHECK-LABEL: sil hidden [ossa] @$s13auto_closures05call_A8_closureyAA4BoolVADyXKF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> Bool) -> Bool
func call_auto_closure(_ x: @autoclosure () -> Bool) -> Bool {
  // CHECK: bb0([[CLOSURE:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> Bool):
  // CHECK: [[CLOSUREC:%.*]] = copy_value [[CLOSURE]]
  // CHECK: [[CLOSUREB:%.*]] = begin_borrow [[CLOSUREC]]
  // CHECK: [[RET:%.*]] = apply [[CLOSUREB]]()
  // CHECK: return [[RET]]
  return x()
}

// CHECK-LABEL: sil hidden [ossa] @$s13auto_closures05test_A21_closure_with_capture{{[_0-9a-zA-Z]*}}F
func test_auto_closure_with_capture(_ x: Bool) -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @$s13auto_closures05test_A21_closure_with_capture
  // CHECK: [[WITHCAPTURE:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE]](
  // CHECK: [[CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[WITHCAPTURE]]
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[CVT]])
  // CHECK: return [[RET]]
  return call_auto_closure(x)
}

// CHECK-LABEL: sil hidden [ossa] @$s13auto_closures05test_A24_closure_without_capture{{[_0-9a-zA-Z]*}}F
func test_auto_closure_without_capture() -> Bool {
  // CHECK: [[CLOSURE:%.*]] = function_ref @$s13auto_closures05test_A24_closure_without_capture
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) () -> Bool to $@noescape @callee_guaranteed () -> Bool
  // CHECK: [[RET:%.*]] = apply {{%.*}}([[THICK]])
  // CHECK: return [[RET]]
  return call_auto_closure(false_)
}

public class Base {
  var x: Bool { return false_ }
}

public class Sub : Base {
  // CHECK-LABEL: sil hidden [ossa] @$s13auto_closures3SubC1xAA4BoolVvg : $@convention(method) (@guaranteed Sub) -> Bool {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Sub):
  // CHECK: [[AUTOCLOSURE_FUNC:%.*]] = function_ref @$s13auto_closures3SubC1xAA4BoolVvgAFyXEfu_ : $@convention(thin) (@guaranteed Sub) -> Bool
  // CHECK: [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK: [[AUTOCLOSURE:%.*]] = partial_apply [callee_guaranteed] [[AUTOCLOSURE_FUNC]]([[SELF_COPY]])
  // CHECK: [[CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[AUTOCLOSURE]]
  // CHECK: [[AUTOCLOSURE_CONSUMER:%.*]] = function_ref @$s13auto_closures05call_A8_closureyAA4BoolVADyXKF : $@convention(thin)
  // CHECK: [[RET:%.*]] = apply [[AUTOCLOSURE_CONSUMER]]([[CVT]])
  // CHECK: return [[RET]] : $Bool
  // CHECK: }

  // CHECK-LABEL: sil private [transparent] [ossa] @$s13auto_closures3SubC1xAA4BoolVvgAFyXEfu_ : $@convention(thin) (@guaranteed Sub) -> Bool {
  // CHECK: [[SUPER:%[0-9]+]] = function_ref @$s13auto_closures4BaseC1xAA4BoolVvg : $@convention(method) (@guaranteed Base) -> Bool
  // CHECK: [[RET:%.*]] = apply [[SUPER]]({{%.*}})
  // CHECK: return [[RET]]
  override var x: Bool { return call_auto_closure(super.x) }
}

// CHECK-LABEL: sil hidden [ossa] @$s13auto_closures20closureInAutoclosureyAA4BoolVAD_ADtF : $@convention(thin) (Bool, Bool) -> Bool {
// CHECK: }
// CHECK-LABEL: sil private [transparent] [ossa] @$s13auto_closures20closureInAutoclosureyAA4BoolVAD_ADtFADyXEfu_ : $@convention(thin) (Bool, Bool) -> Bool {
// CHECK: }
// CHECK-LABEL: sil private [ossa] @$s13auto_closures20closureInAutoclosureyAA4BoolVAD_ADtFADyXEfu_A2DXEfU_ : $@convention(thin) (Bool, Bool) -> Bool {
// CHECK: }
func compareBool(_ lhs: Bool, _ rhs: Bool) -> Bool { return false_ }
func testBool(_ x: Bool, _ pred: (Bool) -> Bool) -> Bool {
  return pred(x)
}
func delayBool(_ fn: @autoclosure () -> Bool) -> Bool {
  return fn()
}
func closureInAutoclosure(_ lhs: Bool, _ rhs: Bool) -> Bool {
  return delayBool(testBool(lhs, { compareBool($0, rhs) }))
}
