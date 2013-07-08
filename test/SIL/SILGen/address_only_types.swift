// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

protocol Unloadable {
  func foo() -> Int
  var address_only_prop : Unloadable
  var loadable_prop : Int
}

// CHECK: sil @_T18address_only_types21address_only_argumentFT1xPS_10Unloadable__T_
func address_only_argument(x:Unloadable) {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $*Unloadable):
  // CHECK: destroy_addr [[XADDR]]
  // CHECK-NOT: dealloc_var {{.*}} [[XADDR]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types30address_only_curried_argumentsfT1xPS_10Unloadable__FT1yPS0___T_
func address_only_curried_arguments(x:Unloadable)(y:Unloadable) {
  // CHECK: bb0([[YADDR:%[0-9]+]] : $*Unloadable, [[XADDR:%[0-9]+]] : $*Unloadable):
  // CHECK: destroy_addr [[XADDR]]
  // CHECK: destroy_addr [[YADDR]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types41address_only_curried_arguments_and_returnfT1xPS_10Unloadable__FT1yPS0___PS0__
func address_only_curried_arguments_and_return(x:Unloadable)(y:Unloadable) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[YADDR:%[0-9]+]] : $*Unloadable, [[XADDR:%[0-9]+]] : $*Unloadable):
  return x
  // CHECK: copy_addr [[XADDR]] to [[RET]] [initialization]
  // CHECK: destroy_addr [[XADDR]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types19address_only_returnFT1xPS_10Unloadable_1ySi_PS0__
func address_only_return(x:Unloadable, y:Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[XADDR:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $Int64):
  return x
  // CHECK: copy_addr [[XADDR]] to [[RET]] [initialization]
  // CHECK: [[VOID:%[0-9]+]] = tuple ()
  // CHECK: [[VOID]]
}

// CHECK: sil @_T18address_only_types27address_only_curried_returnfT1xPS_10Unloadable__FT1ySi_PS0__
func address_only_curried_return(x:Unloadable)(y:Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $Int64, [[XADDR:%[0-9]+]] : $*Unloadable):
  return x
  // CHECK: copy_addr [[XADDR]] to [[RET]] [initialization]
  // CHECK: [[VOID:%[0-9]+]] = tuple ()
  // CHECK: [[VOID]]
}

// CHECK: sil @_T18address_only_types27byref_address_only_argumentFT1xRPS_10Unloadable__T_
func byref_address_only_argument(x:[byref] Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_var
  // CHECK-NOT: copy_addr
  // CHECK: return
}

func some_address_only_function_1() -> Unloadable {}
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
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_var stack [[TEMP]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types19address_only_call_2FT1xPS_10Unloadable__T_
func address_only_call_2(x:Unloadable) {
  // CHECK: bb0([[XADDR:%[0-9]+]] : $*Unloadable):
  some_address_only_function_2(x)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_2FT1xPS_10Unloadable__T_
  // CHECK: [[X_CALL_ARG:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: copy_addr [[XADDR]] to [[X_CALL_ARG]] [initialization]
  // CHECK: apply [[FUNC]]([[X_CALL_ARG]])
  // CHECK: dealloc_var stack [[X_CALL_ARG]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types24address_only_call_1_in_2FT_T_
func address_only_call_1_in_2() {
  // CHECK: bb0:
  some_address_only_function_2(some_address_only_function_1())
  // CHECK: [[FUNC2:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_2FT1xPS_10Unloadable__T_
  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: apply [[FUNC1]]([[TEMP]])
  // CHECK: apply [[FUNC2]]([[TEMP]])
  // CHECK: dealloc_var stack [[TEMP]]
  // CHECK: return
}

// CHECK: sil @_T18address_only_types24address_only_materializeFT_Si
func address_only_materialize() -> Int {
  // CHECK: bb0:
  return some_address_only_function_1().foo()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: [[TEMP_PROJ:%[0-9]+]] = project_existential [[TEMP]]
  // CHECK: [[FOO_METHOD:%[0-9]+]] = protocol_method [[TEMP]] : {{.*}}, #Unloadable.foo!1
  // CHECK: [[RET:%[0-9]+]] = apply [[FOO_METHOD]]([[TEMP_PROJ]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_var stack [[TEMP]]
  // CHECK: return [[RET]]
}

// CHECK: sil @_T18address_only_types33address_only_assignment_from_tempFT4destRPS_10Unloadable__T_
func address_only_assignment_from_temp(dest:[byref] Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable):
  dest = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: copy_addr [[TEMP]] [take] to [[DEST]]{{$}}
  // CHECK-NOT: destroy_addr [[TEMP]]
  // CHECK: dealloc_var stack [[TEMP]]
}

// CHECK: sil @_T18address_only_types31address_only_assignment_from_lvFT4destRPS_10Unloadable_1vPS0___T_
func address_only_assignment_from_lv(dest:[byref] Unloadable, v:Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable, [[VADDR:%[0-9]+]] : $*Unloadable):
  dest = v
  // FIXME: emit into?
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: copy_addr [[VADDR]] to [[TEMP]] [initialization]
  // CHECK: copy_addr [[TEMP]] [take] to [[DEST]]{{$}}
}

var global_prop : Unloadable { get: set: }

// CHECK: sil @_T18address_only_types45address_only_assignment_from_temp_to_propertyFT_T_
func address_only_assignment_from_temp_to_property() {
  // CHECK: bb0:
  global_prop = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_T18address_only_types11global_propPS_10Unloadable_s
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_var stack [[TEMP]]
}

// CHECK: sil @_T18address_only_types43address_only_assignment_from_lv_to_propertyFT1vPS_10Unloadable__T_
func address_only_assignment_from_lv_to_property(v:Unloadable) {
  // CHECK: bb0([[VADDR:%[0-9]+]] : $*Unloadable):
  // CHECK: [[TEMP:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: copy_addr [[VADDR]] to [[TEMP]] [initialization]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_T18address_only_types11global_propPS_10Unloadable_s
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_var stack [[TEMP]]
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
  // CHECK: [[XADDR:%[0-9]+]] = alloc_var stack $Unloadable
  // CHECK: apply {{%.*}}([[XADDR]])
  return x
  // CHECK: copy_addr [[XADDR]] to [[RET]] [initialization]
  // CHECK: dealloc_var stack [[XADDR]]
  // CHECK: return
}

func unloadable_to_unloadable(x : Unloadable) -> Unloadable { }
var some_address_only_nontuple_arg_function : (Unloadable) -> Unloadable = unloadable_to_unloadable

// CHECK: sil @_T18address_only_types39call_address_only_nontuple_arg_functionFT1xPS_10Unloadable__T_
func call_address_only_nontuple_arg_function(x:Unloadable) {
  some_address_only_nontuple_arg_function(x)
}

// CHECK: sil @_T18address_only_types22address_only_tuple_argFT1xTPS_10Unloadable_PS0____PS0__
func address_only_tuple_arg(x:(Unloadable, Unloadable)) -> Unloadable {
  // CHECK: bb0([[RET:%.*]] : $*Unloadable, [[X0ARG:%.*]] : $*Unloadable, [[X1ARG:%.*]] : $*Unloadable):
  // CHECK: [[XADDR:%.*]] = alloc_var stack $(Unloadable, Unloadable)
  // CHECK: [[X0ADDR:%.*]] = tuple_element_addr [[XADDR]], 0
  // CHECK: copy_addr [[X0ARG]] [take] to [[X0ADDR]] [initialization]
  // CHECK: [[X1ADDR:%.*]] = tuple_element_addr [[XADDR]], 1
  // CHECK: copy_addr [[X1ARG]] [take] to [[X1ADDR]] [initialization]

  return x.1
  // CHECK: [[X1ADDR:%.*]] = tuple_element_addr [[XADDR]], 1
  // CHECK: copy_addr [[X1ADDR]] to [[RET]] [initialization]
}

// CHECK: sil @_T18address_only_types25address_only_closure_exprFT_FPS_10Unloadable_PS0__
func address_only_closure_expr() -> (Unloadable) -> Unloadable {
  return {$0}
}

// CHECK: sil internal @closure{{.*}}
// CHECK: bb0([[TO:%.*]] : $*Unloadable, [[FROM:%.*]] : $*Unloadable):
// CHECK: copy_addr [[FROM]] to [[TO]] [initialization]
// CHECK: destroy_addr [[FROM]]

// CHECK: sil @_T18address_only_types28address_only_closure_captureFT1xPS_10Unloadable__FT_PS0__
func address_only_closure_capture(x:Unloadable) -> () -> (Unloadable) {
  // CHECK: bb0([[XARG:%.*]] : $*Unloadable):
  // CHECK: [[XBOX:%.*]] = alloc_box $Unloadable
  // CHECK: copy_addr [[XARG]] [take] to [[XBOX]]#1 [initialization]
  // CHECK: [[CLOSURE:%.*]] = function_ref [[CLOSURE_FUNC:@closure[0-9]+]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Unloadable)) -> Unloadable
  // CHECK: [[CLOSURE_OBJ:%.*]] = partial_apply [[CLOSURE]]([[XBOX]]#0, [[XBOX]]#1)
  // CHECK: return [[CLOSURE_OBJ]]
  return {x}
}

// CHECK: sil internal [[CLOSURE_FUNC]] : $[thin] ((), (Builtin.ObjectPointer, [byref] Unloadable)) -> Unloadable
// CHECK: bb0([[RET:%.*]] : $*Unloadable, [[XBOX_OWNER:%.*]] : $Builtin.ObjectPointer, [[XBOX_ADDR:%.*]] : $*Unloadable):
// CHECK:   copy_addr [[XBOX_ADDR]] to [[RET]] [initialization]
// CHECK:   release [[XBOX_OWNER]]

