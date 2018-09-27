
// RUN: %target-swift-emit-silgen -module-name downcast_reabstraction -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s22downcast_reabstraction19condFunctionFromAnyyyypF
// CHECK:         checked_cast_addr_br take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed () -> @out (), [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$sytIegr_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[ORIG_VAL]])

func condFunctionFromAny(_ x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden @$s22downcast_reabstraction21uncondFunctionFromAnyyyypF : $@convention(thin) (@in_guaranteed Any) -> () {
// CHECK:         unconditional_checked_cast_addr Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed () -> @out ()
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @$sytIegr_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[ORIG_VAL]])
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUBST_VAL]]
// CHECK:         apply [[BORROW]]()
// CHECK:         end_borrow [[BORROW]]
// CHECK:         destroy_value [[SUBST_VAL]]
func uncondFunctionFromAny(_ x: Any) {
  (x as! () -> ())()
}
