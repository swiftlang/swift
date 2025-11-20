
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name address_only_types -parse-as-library -parse-stdlib %s | %FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

typealias Int = Builtin.Int64

enum Bool { case true_, false_ }

protocol Unloadable {
  func foo() -> Int
  var address_only_prop : Unloadable { get }
  var loadable_prop : Int { get }
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B9_argument{{[_0-9a-zA-Z]*}}F
func address_only_argument(_ x: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*any Unloadable):
  // CHECK: debug_value [[XARG]] {{.*}} expr op_deref
  // CHECK-NEXT: tuple
  // CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B17_ignored_argument{{[_0-9a-zA-Z]*}}F
func address_only_ignored_argument(_: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*any Unloadable):
  // CHECK-NOT: dealloc_stack {{.*}} [[XARG]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B7_return{{[_0-9a-zA-Z]*}}F
func address_only_return(_ x: Unloadable, y: Int) -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*any Unloadable, [[XARG:%[0-9]+]] : $*any Unloadable, [[YARG:%[0-9]+]] : $Builtin.Int64):
  // CHECK-NEXT: debug_value [[XARG]] : $*any Unloadable, let, name "x", {{.*}} expr op_deref
  // CHECK-NEXT: debug_value [[YARG]] : $Builtin.Int64, let, name "y"
  // CHECK-NEXT: copy_addr [[XARG]] to [init] [[RET]]
  // CHECK-NEXT: [[VOID:%[0-9]+]] = tuple ()
  // CHECK-NEXT: return [[VOID]]
  return x
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B15_missing_return{{[_0-9a-zA-Z]*}}F
func address_only_missing_return() -> Unloadable {
  // CHECK: unreachable
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B27_conditional_missing_return{{[_0-9a-zA-Z]*}}F
func address_only_conditional_missing_return(_ x: Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*any Unloadable, {{%.*}} : $*any Unloadable):
  // CHECK:   switch_enum {{%.*}}, case #Bool.true_!enumelt: [[TRUE:bb[0-9]+]], case #Bool.false_!enumelt: [[FALSE:bb[0-9]+]]
  switch Bool.true_ {
  case .true_:
  // CHECK: [[TRUE]]:
    // CHECK:   copy_addr %1 to [init] %0 : $*any Unloadable
  // CHECK:   return
    return x
  case .false_:
    ()
  }
  // CHECK: [[FALSE]]:
  // CHECK:   unreachable
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B29_conditional_missing_return_2
func address_only_conditional_missing_return_2(_ x: Unloadable) -> Unloadable {
  // CHECK: bb0({{%.*}} : $*any Unloadable, {{%.*}} : $*any Unloadable):
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

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B7_call_1
func address_only_call_1() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*any Unloadable):
  return some_address_only_function_1()
  // FIXME emit into
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_1AA10Unloadable_pyF
  // CHECK: apply [[FUNC]]([[RET]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B21_call_1_ignore_returnyyF
func address_only_call_1_ignore_return() {
  // CHECK: bb0:
  some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_1AA10Unloadable_pyF
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B7_call_2{{[_0-9a-zA-Z]*}}F
func address_only_call_2(_ x: Unloadable) {
  // CHECK: bb0([[XARG:%[0-9]+]] : $*any Unloadable):
  // CHECK: debug_value [[XARG]] : $*any Unloadable, {{.*}} expr op_deref
  some_address_only_function_2(x)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_2{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC]]([[XARG]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B12_call_1_in_2{{[_0-9a-zA-Z]*}}F
func address_only_call_1_in_2() {
  // CHECK: bb0:
  some_address_only_function_2(some_address_only_function_1())
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_1{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC1]]([[TEMP]])
  // CHECK: [[FUNC2:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_2{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC2]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B12_materialize{{[_0-9a-zA-Z]*}}F
func address_only_materialize() -> Int {
  // CHECK: bb0:
  return some_address_only_function_1().foo()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @$s18address_only_types05some_a1_B11_function_1{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC]]([[TEMP]])
  // CHECK: [[TEMP_PROJ:%[0-9]+]] = open_existential_addr immutable_access [[TEMP]] : $*any Unloadable to $*[[OPENED:@opened\(.*, any Unloadable\) Self]]
  // CHECK: [[FOO_METHOD:%[0-9]+]] = witness_method $[[OPENED]], #Unloadable.foo :
  // CHECK: [[RET:%[0-9]+]] = apply [[FOO_METHOD]]<[[OPENED]]>([[TEMP_PROJ]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B21_assignment_from_temp{{[_0-9a-zA-Z]*}}F
func address_only_assignment_from_temp(_ dest: inout Unloadable) {
  // CHECK: bb0([[DEST:%[0-9]+]] : $*any Unloadable):
  dest = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: %[[ACCESS:.*]] = begin_access [modify] [unknown] %0 :
  // CHECK: copy_addr [take] [[TEMP]] to %[[ACCESS]] :
  // CHECK-NOT: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B19_assignment_from_lv{{[_0-9a-zA-Z]*}}F
func address_only_assignment_from_lv(_ dest: inout Unloadable, v: Unloadable) {
  var v = v
  // CHECK: bb0([[DEST:%[0-9]+]] : $*any Unloadable, [[VARG:%[0-9]+]] : $*any Unloadable):
  // CHECK: [[VBOX:%.*]] = alloc_box ${ var any Unloadable }
  // CHECK: [[V_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VBOX]]
  // CHECK: [[PBOX:%[0-9]+]] = project_box [[V_LIFETIME]]
  // CHECK: copy_addr [[VARG]] to [init] [[PBOX]] : $*any Unloadable
  dest = v
  // CHECK: [[READBOX:%.*]] = begin_access [read] [unknown] [[PBOX]] :
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: copy_addr [[READBOX]] to [init] [[TEMP]] :
  // CHECK: [[RET:%.*]] = begin_access [modify] [unknown] %0 :
  // CHECK: copy_addr [take] [[TEMP]] to [[RET]] :
  // CHECK: destroy_value [[VBOX]]
}

var global_prop : Unloadable {
  get {
    return crap
  }
  set {}
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B33_assignment_from_temp_to_property{{[_0-9a-zA-Z]*}}F
func address_only_assignment_from_temp_to_property() {
  // CHECK: bb0:
  global_prop = some_address_only_function_1()
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @$s18address_only_types11global_propAA10Unloadable_pvs
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B31_assignment_from_lv_to_property{{[_0-9a-zA-Z]*}}F
func address_only_assignment_from_lv_to_property(_ v: Unloadable) {
  // CHECK: bb0([[VARG:%[0-9]+]] : $*any Unloadable):
  // CHECK: debug_value [[VARG]] : $*any Unloadable, {{.*}} expr op_deref
  // CHECK: [[TEMP:%[0-9]+]] = alloc_stack $any Unloadable
  // CHECK: copy_addr [[VARG]] to [init] [[TEMP]]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @$s18address_only_types11global_propAA10Unloadable_pvs
  // CHECK: apply [[SETTER]]([[TEMP]])
  // CHECK: dealloc_stack [[TEMP]]
  global_prop = v
}

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types0a1_B4_varAA10Unloadable_pyF
func address_only_var() -> Unloadable {
  // CHECK: bb0([[RET:%[0-9]+]] : $*any Unloadable):
  var x = some_address_only_function_1()
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box ${ var any Unloadable }
  // CHECK: [[XBOX_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[XBOX]]
  // CHECK: [[XPB:%.*]] = project_box [[XBOX_LIFETIME]]
  // CHECK: apply {{%.*}}([[XPB]])
  return x
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[XPB]] :
  // CHECK: copy_addr [[ACCESS]] to [init] %0
  // CHECK: destroy_value [[XBOX]]
  // CHECK: return
}

func unloadable_to_unloadable(_ x: Unloadable) -> Unloadable { return x }
var some_address_only_nontuple_arg_function : (Unloadable) -> Unloadable = unloadable_to_unloadable

// CHECK-LABEL: sil hidden [ossa] @$s18address_only_types05call_a1_B22_nontuple_arg_function{{[_0-9a-zA-Z]*}}F
func call_address_only_nontuple_arg_function(_ x: Unloadable) {
  some_address_only_nontuple_arg_function(x)
}
