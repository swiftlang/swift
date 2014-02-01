// RUN: %swift -emit-silgen %s | FileCheck %s

class X {
  // CHECK: sil @_TFC12dynamic_self1X1ffDS0_FT_DS0_ : $@cc(method) @thin (@owned X) -> @owned
  func f() -> DynamicSelf { return self }
}

class Y : X { }

// CHECK-LABEL: sil @_TF12dynamic_self23testDynamicSelfDispatchFT1yCS_1Y_T_ : $@thin (@owned Y) -> ()
func testDynamicSelfDispatch(y: Y) {
  // CHECK: bb0([[Y:%[0-9]+]] : $Y):
  // CHECK:   [[X_F_REF:%[0-9]+]] = function_ref @_TFC12dynamic_self1X1fFDS0_FT_DS0_ : $@thin (@owned X) -> @owned @callee_owned () -> @owned X
  // CHECK:   [[THICK_X_F_REF:%[0-9]+]] = thin_to_thick_function [[X_F_REF]] : $@thin (@owned X) -> @owned @callee_owned () -> @owned X to $@callee_owned (@owned X) -> @owned @callee_owned () -> @owned X
// CHECK:   [[X_F_REF_AS_Y:%[0-9]+]] = convert_function [[THICK_X_F_REF]] : $@callee_owned (@owned X) -> @owned @callee_owned () -> @owned X to $@callee_owned (@owned X) -> @owned @callee_owned () -> @owned Y
// CHECK:   strong_retain [[Y]] : $Y
// CHECK:   [[Y_AS_X:%[0-9]+]] = upcast [[Y]] : $Y to $X
// CHECK:   [[Y_REF_BOUND:%[0-9]+]] = apply [[X_F_REF_AS_Y]]([[Y_AS_X]]) : $@callee_owned (@owned X) -> @owned @callee_owned () -> @owned Y
// CHECK:   [[Y_RESULT:%[0-9]+]] = apply [[Y_REF_BOUND]]() : $@callee_owned () -> @owned Y
  y.f()
}
