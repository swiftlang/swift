// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T022downcast_reabstraction19condFunctionFromAnyyypF 
// CHECK:         checked_cast_addr_br take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_owned (@in ()) -> @out (), [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
// CHECK:       [[YES]]:
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_T0ytytIxir_Ix_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])

func condFunctionFromAny(_ x: Any) {
  if let f = x as? () -> () {
    f()
  }
}

// CHECK-LABEL: sil hidden @_T022downcast_reabstraction21uncondFunctionFromAnyyypF : $@convention(thin) (@in Any) -> () {
// CHECK:         unconditional_checked_cast_addr take_always Any in [[IN:%.*]] : $*Any to () -> () in [[OUT:%.*]] : $*@callee_owned (@in ()) -> @out ()
// CHECK:         [[ORIG_VAL:%.*]] = load [take] [[OUT]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref @_T0ytytIxir_Ix_TR
// CHECK:         [[SUBST_VAL:%.*]] = partial_apply [[REABSTRACT]]([[ORIG_VAL]])
// CHECK:         apply [[SUBST_VAL]]()
func uncondFunctionFromAny(_ x: Any) {
  (x as! () -> ())()
}
