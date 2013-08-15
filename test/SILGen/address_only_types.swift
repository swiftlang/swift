// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

protocol Unloadable {
  func foo() -> Int
  var address_only_prop : Unloadable
  var loadable_prop : Int
}

// CHECK: sil @_T18address_only_types21address_only_argumentFT1xPS_10Unloadable__T_
func address_only_argument(x:Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[XARG]] to [initialization] [[XBOX]]#1
  // CHECK: release [[XBOX]]#0
  // CHECK-NOT: dealloc_stack {{.*}} [[XARG]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types29address_only_ignored_argumentFPS_10Unloadable_T_
func address_only_ignored_argument(_:Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: destroy_addr [[XARG]]
  // CHECK-NOT: dealloc_stack {{.*}} [[XARG]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types30address_only_curried_argumentsfT1xPS_10Unloadable__FT1yPS0___T_
func address_only_curried_arguments(x:Unloadable)(y:Unloadable) {
  // CHECK: bb0([[YARG:%[0-9]+]] : $*Unloadable, [[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[YBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[YARG]] to [initialization] [[YBOX]]#1
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[XARG]] to [initialization] [[XBOX]]#1
  // CHECK: release [[XBOX]]#0
  // CHECK: release [[YBOX]]#0
  // CHECK: return
}

// CHECK: sil @_T18address_only_types41address_only_curried_arguments_and_returnfT1xPS_10Unloadable__FT1yPS0___PS0__
func address_only_curried_arguments_and_return(x:Unloadable)(y:Unloadable) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $*Unloadable, [[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[YBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[YARG]] to [initialization] [[YBOX]]#1
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[XARG]] to [initialization] [[XBOX]]#1
  return x
  // CHECK: copy_addr [[XBOX]]#1 to [initialization] [[RET]]
  // CHECK: release [[XBOX]]#0
  // CHECK: release [[YBOX]]#0
  // CHECK: return
}

// CHECK: sil @_T18address_only_types19address_only_returnFT1xPS_10Unloadable_1ySi_PS0__
func address_only_return(x:Unloadable, y:Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[XARG:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $Int64):
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[XARG]] to [initialization] [[XBOX]]#1
  return x
  // CHECK: copy_addr [[XBOX]]#1 to [initialization] [[RET]]
  // CHECK: [[VOID:%[0-9]+]] = tuple ()
  // CHECK: [[VOID]]
}

// CHECK: sil @_T18address_only_types27address_only_curried_returnfT1xPS_10Unloadable__FT1ySi_PS0__
func address_only_curried_return(x:Unloadable)(y:Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $Int64, [[XADDR:%[0-9]+]] : $*Unloadable):
  return x
  // CHECK: copy_addr {{%.*}} to [initialization] [[RET]]
  // CHECK: [[VOID:%[0-9]+]] = tuple ()
  // CHECK: [[VOID]]
}

// CHECK: sil @_T18address_only_types27address_only_missing_returnFT_PS_10Unloadable_
func address_only_missing_return() -> Unloadable {
  // CHECK: unreachable
}

// CHECK: sil @_T18address_only_types39address_only_conditional_missing_returnFT1xPS_10Unloadable__PS0__
func address_only_conditional_missing_return(x:Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*Unloadable, {{%.*}} : $*Unloadable):
  // CHECK:   [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK:   condbranch {{%.*}}, [[TRUE:bb[0-9]+]], [[FALSE:bb[0-9]+]]
  if true {
  // CHECK: [[TRUE]]:
  // CHECK:   release [[XBOX]]#0
  // CHECK:   return
    return x
  }
  // CHECK: [[FALSE]]:
  // CHECK:   unreachable
}

// CHECK: sil @_T18address_only_types41address_only_conditional_missing_return_2FT1xPS_10Unloadable__PS0__
func address_only_conditional_missing_return_2(x:Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*Unloadable, {{%.*}} : $*Unloadable):
  // CHECK:   [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK:   condbranch {{%.*}}, [[TRUE1:bb[0-9]+]], [[FALSE1:bb[0-9]+]]
  if true {
  // CHECK: [[TRUE1]]:
  // CHECK:   br [[EPILOG:bb[0-9]+]]
    return x
  }
  // CHECK: [[FALSE1]]:
  // CHECK:   condbranch {{%.*}}, [[TRUE2:bb[0-9]+]], [[FALSE2:bb[0-9]+]]
  if false {
  // CHECK: [[TRUE2]]:
  // CHECK:   br [[EPILOG]]
    return x
  }
  // CHECK: [[FALSE2]]:
  // CHECK:   unreachable

  // CHECK: [[EPILOG]]:
  // CHECK:   release [[XBOX]]#0
  // CHECK:   return
}

var crap : Unloadable
func some_address_only_function_1() -> Unloadable { return crap }
func some_address_only_function_2(x:Unloadable) -> () {}

// CHECK: sil @_T18address_only_types19address_only_call_1FT_PS_10Unloadable_
func address_only_call_1() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable):
  return some_address_only_function_1()
  // FIXME emit into
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: apply [[FUNC]]([[RET]])
  // CHECK: return
}

// CHECK: sil @_T18address_only_types33address_only_call_1_ignore_returnFT_T_
func address_only_call_1_ignore_return() {
  // CHECK: bb0:
  some_address_only_function_1()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]]#1)
  // CHECK: destroy_addr [[TEMP]]#1
  // CHECK: dealloc_stack [[TEMP]]#0
  // CHECK: return
}

// CHECK: sil @_T18address_only_types19address_only_call_2FT1xPS_10Unloadable__T_
func address_only_call_2(x:Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  some_address_only_function_2(x)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_2FT1xPS_10Unloadable__T_
  // CHECK: [[X_CALL_ARG:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [[XBOX]]#1 to [initialization] [[X_CALL_ARG]]
  // CHECK: apply [[FUNC]]([[X_CALL_ARG]]#1)
  // CHECK: dealloc_stack [[X_CALL_ARG]]#0
  // CHECK: return
}

// CHECK: sil @_T18address_only_types24address_only_call_1_in_2FT_T_
func address_only_call_1_in_2() {
  // CHECK: bb0:
  some_address_only_function_2(some_address_only_function_1())
  // CHECK: [[FUNC2:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_2FT1xPS_10Unloadable__T_
  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC1]]([[TEMP]]#1)
  // CHECK: apply [[FUNC2]]([[TEMP]]#1)
  // CHECK: dealloc_stack [[TEMP]]#0
  // CHECK: return
}

// CHECK: sil @_T18address_only_types24address_only_materializeFT_Si
func address_only_materialize() -> Int {
  // CHECK: bb0:
  return some_address_only_function_1().foo()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]]#1)
  // CHECK: [[TEMP_PROJ:%[0-9]+]] = project_existential [[TEMP]]#1
  // CHECK: [[FOO_METHOD:%[0-9]+]] = protocol_method [[TEMP]]#1 : {{.*}}, #Unloadable.foo!1
  // CHECK: [[RET:%[0-9]+]] = apply [[FOO_METHOD]]([[TEMP_PROJ]])
  // CHECK: destroy_addr [[TEMP]]#1
  // CHECK: dealloc_stack [[TEMP]]#0
  // CHECK: return [[RET]]
}

// CHECK: sil @_T18address_only_types33address_only_assignment_from_tempFT4destRPS_10Unloadable__T_
func address_only_assignment_from_temp(dest:[byref] Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable):
  dest = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [take] [[TEMP]]#1 to [[DEST]] :
  // CHECK-NOT: destroy_addr [[TEMP]]#1
  // CHECK: dealloc_stack [[TEMP]]#0
}

// CHECK: sil @_T18address_only_types31address_only_assignment_from_lvFT4destRPS_10Unloadable_1vPS0___T_
func address_only_assignment_from_lv(dest:[byref] Unloadable, v:Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable, [[VARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[VBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[VARG]] to [initialization] [[VBOX]]#1
  dest = v
  // FIXME: emit into?
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [[VBOX]]#1 to [initialization] [[TEMP]]
  // CHECK: copy_addr [take] [[TEMP]]#1 to [[DEST]] :
}

var global_prop : Unloadable { get: return crap set: }

// CHECK: sil @_T18address_only_types45address_only_assignment_from_temp_to_propertyFT_T_
func address_only_assignment_from_temp_to_property() {
  // CHECK: bb0:
  global_prop = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_T18address_only_types11global_propPS_10Unloadable_s
  // CHECK: apply [[SETTER]]([[TEMP]]#1)
  // CHECK: dealloc_stack [[TEMP]]#0
}

// CHECK: sil @_T18address_only_types43address_only_assignment_from_lv_to_propertyFT1vPS_10Unloadable__T_
func address_only_assignment_from_lv_to_property(v:Unloadable) {
  // CHECK: bb0([[VARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[VBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [take] [[VARG]] to [initialization] [[VBOX]]#1
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [[VBOX]]#1 to [initialization] [[TEMP]]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_T18address_only_types11global_propPS_10Unloadable_s
  // CHECK: apply [[SETTER]]([[TEMP]]#1)
  // CHECK: dealloc_stack [[TEMP]]#0
  global_prop = v
}

/* TODO: address-only writeback
   needs support for archetype/existential property member refs
// TODO: sil @address_only_assignment_to_property_with_writeback
func address_only_assignment_to_property_with_writeback(v:Unloadable) {
  global_prop.address_only_prop.loadable_prop = 1
  global_prop.address_only_prop.address_only_prop = v
}
*/

// CHECK: sil @_T18address_only_types16address_only_varFT_PS_10Unloadable_
func address_only_var() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable):
  var x = some_address_only_function_1()
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Unloadable
  // CHECK: apply {{%.*}}([[XBOX]]#1)
  return x
  // CHECK: copy_addr [[XBOX]]#1 to [initialization] [[RET]]
  // CHECK: release [[XBOX]]#0
  // CHECK: return
}

func unloadable_to_unloadable(x : Unloadable) -> Unloadable { return x }
var some_address_only_nontuple_arg_function : (Unloadable) -> Unloadable = unloadable_to_unloadable

// CHECK: sil @_T18address_only_types39call_address_only_nontuple_arg_functionFT1xPS_10Unloadable__T_
func call_address_only_nontuple_arg_function(x:Unloadable) {
  some_address_only_nontuple_arg_function(x)
}
