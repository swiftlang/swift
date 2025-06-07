
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name downcast_reabstraction %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s22downcast_reabstraction19condFunctionFromAnyyyypF
// CHECK:         checked_cast_addr_br take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[CONV_VAL:%.*]] = convert_function [[ORIG_VAL]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$sytIegr_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[CONV_VAL]])

func condFunctionFromAny(_ x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s22downcast_reabstraction21uncondFunctionFromAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> () {
// CHECK:         unconditional_checked_cast_addr Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[CONV_VAL:%.*]] = convert_function [[ORIG_VAL]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$sytIegr_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[CONV_VAL]])
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUBST_VAL]]
// CHECK:         apply [[BORROW]]()
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[SUBST_VAL]]
func uncondFunctionFromAny(_ x: Any) {
  (x as! () -> ())()
}
