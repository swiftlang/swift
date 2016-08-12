// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_TF22downcast_reabstraction19condFunctionFromAnyFP_T_ 
// CHECK:         checked_cast_addr_br take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_owned (@in ()) -> @out (), [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo___
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])

func condFunctionFromAny(_ x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden @_TF22downcast_reabstraction21uncondFunctionFromAnyFP_T_ : $@convention(thin) (@in Any) -> () {
// CHECK:         unconditional_checked_cast_addr take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_owned (@in ()) -> @out ()
// CHECK:         [[ORIG_VAL:%.*]] = load [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo___
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])
// CHECK:         apply [[SUBST_VAL]]()
func uncondFunctionFromAny(_ x: Any) {
  (x as! () -> ())()
}
