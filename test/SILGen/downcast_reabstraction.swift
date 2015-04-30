// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TF22downcast_reabstraction19condFunctionFromAnyFP_T_ 
// CHECK:         checked_cast_addr_br take_always protocol<> in [[IN:%.*]]#1 : $*protocol<> to () -> () in [[OUT:%.*]]#1 : $*@callee_owned (@out (), @in ()) -> (), [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [[OUT]]#1
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])

func condFunctionFromAny(x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden @_TF22downcast_reabstraction21uncondFunctionFromAnyFP_T_ : $@convention(thin) (@in protocol<>) -> () {
// CHECK:         unconditional_checked_cast_addr take_always protocol<> in [[IN:%.*]]#1 : $*protocol<> to () -> () in [[OUT:%.*]]#1 : $*@callee_owned (@out (), @in ()) -> ()
// CHECK:         [[ORIG_VAL:%.*]] = load [[OUT]]#1
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_TTRXFo_iT__iT__XFo__dT__
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])
// CHECK:         apply [[SUBST_VAL]]()
func uncondFunctionFromAny(x: Any) {
  (x as! () -> ())()
}
