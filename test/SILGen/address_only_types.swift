// RUN: %target-swift-frontend -parse-as-library -parse-stdlib -emit-silgen %s | FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

typealias Int = Builtin.Int64

enum Bool { case true_, false_ }

protocol Unloadable {
  func foo() -> Int
  var address_only_prop : Unloadable { get }
  var loadable_prop : Int { get }
}

// CHECK-LABEL: sil hidden @_TF18address_only_types21address_only_argument
func address_only_argument(_ x: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: debug_value_addr [[XARG]]
  // CHECK-NEXT: destroy_addr [[XARG]]
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types29address_only_ignored_argument
func address_only_ignored_argument(_: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: destroy_addr [[XARG]]
  // CHECK-NOT: dealloc_stack {{.*}} [[XARG]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types19address_only_return
func address_only_return(_ x: Unloadable, y: Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable, [[XARG:%[0-9]+]] : $*Unloadable, [[YARG:%[0-9]+]] : $Builtin.Int64):
  // CHECK-NEXT: debug_value_addr [[XARG]] : $*Unloadable, let, name "x"
  // CHECK-NEXT: debug_value [[YARG]] : $Builtin.Int64, let, name "y"
  // CHECK-NEXT: copy_addr [take] [[XARG]] to [initialization] [[RET]]
  // CHECK-NEXT: [[VOID:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[VOID]]
  return x
}

// CHECK-LABEL: sil hidden @_TF18address_only_types27address_only_missing_return
func address_only_missing_return() -> Unloadable {
  // CHECK: unreachable
}

// CHECK-LABEL: sil hidden @_TF18address_only_types39address_only_conditional_missing_return
func address_only_conditional_missing_return(_ x: Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*Unloadable, {{%.*}} : $*Unloadable):
  // CHECK:   switch_enum {{%.*}}, case #Bool.true_!enumelt: [[TRUE:bb[0-9]+]], case #Bool.false_!enumelt: [[FALSE:bb[0-9]+]]
  switch Bool.true_ {
  case .true_:
  // CHECK: [[TRUE]]:
  // CHECK:   copy_addr [take] %1 to [initialization] %0 : $*Unloadable
  // CHECK:   return
    return x
  case .false_:
    ()
  }
  // CHECK: [[FALSE]]:
  // CHECK:   unreachable
}

// CHECK-LABEL: sil hidden @_TF18address_only_types41address_only_conditional_missing_return
func address_only_conditional_missing_return_2(_ x: Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*Unloadable, {{%.*}} : $*Unloadable):
  // CHECK:   switch_enum {{%.*}}, case #Bool.true_!enumelt: [[TRUE1:bb[0-9]+]], case #Bool.false_!enumelt: [[FALSE1:bb[0-9]+]]
  switch Bool.true_ {
  case .true_:
    return x
  case .false_:
    ()
  }
  // CHECK: [[FALSE1]]:
  // CHECK:   switch_enum {{%.*}}, case #Bool.true_!enumelt: [[TRUE2:bb[0-9]+]], case #Bool.false_!enumelt: [[FALSE2:bb[0-9]+]]
  switch Bool.true_ {
  case .true_:
    return x
  case .false_:
    ()
  }
  // CHECK: [[FALSE2]]:
  // CHECK:   unreachable

  // CHECK: bb{{.*}}:
  // CHECK:   return
}

var crap : Unloadable = some_address_only_function_1()
func some_address_only_function_1() -> Unloadable { return crap }
func some_address_only_function_2(_ x: Unloadable) -> () {}

// CHECK-LABEL: sil hidden @_TF18address_only_types19address_only_call
func address_only_call_1() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable):
  return some_address_only_function_1()
  // FIXME emit into
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: apply [[FUNC]]([[RET]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types33address_only_call_1_ignore_returnFT_T_
func address_only_call_1_ignore_return() {
  // CHECK: bb0:
  some_address_only_function_1()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_1FT_PS_10Unloadable_
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types19address_only_call_2
func address_only_call_2(_ x: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*Unloadable):
  // CHECK: debug_value_addr [[XARG]] : $*Unloadable
  some_address_only_function_2(x)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_2
  // CHECK: [[X_CALL_ARG:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [[XARG]] to [initialization] [[X_CALL_ARG]]
  // CHECK: apply [[FUNC]]([[X_CALL_ARG]])
  // CHECK: dealloc_stack [[X_CALL_ARG]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types24address_only_call_1_in_2
func address_only_call_1_in_2() {
  // CHECK: bb0:
  some_address_only_function_2(some_address_only_function_1())
  // CHECK: [[FUNC2:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_2
  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_1
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC1]]([[TEMP]])
  // CHECK: apply [[FUNC2]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden @_TF18address_only_types24address_only_materialize
func address_only_materialize() -> Int {
  // CHECK: bb0:
  return some_address_only_function_1().foo()
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF18address_only_types28some_address_only_function_1
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: [[TEMP_PROJ:%[0-9]+]] = open_existential_addr [[TEMP]] : $*Unloadable to $*[[OPENED:@opened(.*) Unloadable]]
  // CHECK: [[FOO_METHOD:%[0-9]+]] = witness_method $[[OPENED]], #Unloadable.foo!1
  // CHECK: [[RET:%[0-9]+]] = apply [[FOO_METHOD]]<[[OPENED]]>([[TEMP_PROJ]])
  // CHECK: destroy_addr [[TEMP_PROJ]]
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @_TF18address_only_types33address_only_assignment_from_temp
func address_only_assignment_from_temp(_ dest: inout Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable):
  // CHECK: [[DEST_LOCAL:%.*]] = alloc_box $Unloadable
  // CHECK: [[PB:%.*]] = project_box [[DEST_LOCAL]]
  // CHECK: copy_addr [[DEST]] to [initialization] [[PB]]
  dest = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [take] [[TEMP]] to [[PB]] :
  // CHECK-NOT: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: copy_addr [[PB]] to [[DEST]]
  // CHECK: release [[DEST_LOCAL]]
}

// CHECK-LABEL: sil hidden @_TF18address_only_types31address_only_assignment_from_lv
func address_only_assignment_from_lv(_ dest: inout Unloadable, v: Unloadable) {
  var v = v
  // CHECK: bb0([[DEST:%[0-9]+]] : $*Unloadable, [[VARG:%[0-9]+]] : $*Unloadable):
  // CHECK: [[DEST_LOCAL:%.*]] = alloc_box $Unloadable
  // CHECK: [[PB:%.*]] = project_box [[DEST_LOCAL]]
  // CHECK: copy_addr [[DEST]] to [initialization] [[PB]]
  // CHECK: [[VBOX:%.*]] = alloc_box $Unloadable
  // CHECK: [[PBOX:%[0-9]+]] = project_box [[VBOX]]
  // CHECK: copy_addr [[VARG]] to [initialization] [[PBOX]] : $*Unloadable
  dest = v
  // FIXME: emit into?
  // CHECK: copy_addr [[PBOX]] to [[PB]] :
  // CHECK: release [[VBOX]]
  // CHECK: copy_addr [[PB]] to [[DEST]]
  // CHECK: release [[DEST_LOCAL]]
}

var global_prop : Unloadable {
  get {
    return crap
  }
  set {}
}

// CHECK-LABEL: sil hidden @_TF18address_only_types45address_only_assignment_from_temp_to_property
func address_only_assignment_from_temp_to_property() {
  // CHECK: bb0:
  global_prop = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_TF18address_only_typess11global_propPS_10Unloadable_
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
}

// CHECK-LABEL: sil hidden @_TF18address_only_types43address_only_assignment_from_lv_to_property
func address_only_assignment_from_lv_to_property(_ v: Unloadable) {
  // CHECK: bb0([[VARG:%[0-9]+]] : $*Unloadable):
  // CHECK: debug_value_addr [[VARG]] : $*Unloadable
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $Unloadable
  // CHECK: copy_addr [[VARG]] to [initialization] [[TEMP]]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_TF18address_only_typess11global_propPS_10Unloadable_
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
  global_prop = v
}

// CHECK-LABEL: sil hidden @_TF18address_only_types16address_only_varFT_PS_10Unloadable_
func address_only_var() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*Unloadable):
  var x = some_address_only_function_1()
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Unloadable
  // CHECK: [[XPB:%.*]] = project_box [[XBOX]]
  // CHECK: apply {{%.*}}([[XPB]])
  return x
  // CHECK: copy_addr [[XPB]] to [initialization] [[RET]]
  // CHECK: release [[XBOX]]
  // CHECK: return
}

func unloadable_to_unloadable(_ x: Unloadable) -> Unloadable { return x }
var some_address_only_nontuple_arg_function : (Unloadable) -> Unloadable = unloadable_to_unloadable

// CHECK-LABEL: sil hidden @_TF18address_only_types39call_address_only_nontuple_arg_function
func call_address_only_nontuple_arg_function(_ x: Unloadable) {
  some_address_only_nontuple_arg_function(x)
}
