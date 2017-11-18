// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T022downcast_reabstraction19condFunctionFromAnyyypF 
// CHECK:         checked_cast_addr_br take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed (@in ()) -> @out (), [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_T0ytytIegir_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[ORIG_VAL]])

func condFunctionFromAny(_ x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden @_T022downcast_reabstraction21uncondFunctionFromAnyyypF : $@convention(thin) (@in Any) -> () {
// CHECK:         unconditional_checked_cast_addr Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_guaranteed (@in ()) -> @out ()
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_T0ytytIegir_Ieg_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACT]]([[ORIG_VAL]])
// CHECK:         [[BORROW:%.*]] = begin_borrow [[SUBST_VAL]]
// CHECK:         apply [[BORROW]]()
// CHECK:         end_borrow [[BORROW]] from [[SUBST_VAL]]
// CHECK:         destroy_value [[SUBST_VAL]]
func uncondFunctionFromAny(_ x: Any) {
  (x as! () -> ())()
}
