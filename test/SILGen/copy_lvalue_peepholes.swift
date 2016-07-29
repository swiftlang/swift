// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

typealias Int = Builtin.Int64

var zero = getInt()
func getInt() -> Int { return zero }

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes20init_var_from_lvalue
// CHECK:   [[X:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[PBX:%.*]] = project_box [[X]]
// CHECK:   [[Y:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[PBY:%.*]] = project_box [[Y]]
// CHECK:   copy_addr [[PBX]] to [initialization] [[PBY]] : $*Builtin.Int64
func init_var_from_lvalue(x: Int) {
  var x = x
  var y = x
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes22assign_var_from_lvalue
// CHECK:   [[X:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[PBX:%.*]] = project_box [[X]]
// CHECK:   [[Y:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[PBY:%.*]] = project_box [[Y]]
// CHECK:   copy_addr [[PBY]] to [[PBX]]
func assign_var_from_lvalue(x: inout Int, y: Int) {
  var y = y
  x = y
}

// -- Peephole doesn't apply to computed lvalues

var computed: Int {
  get {
    return zero
  }
  set {}
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes29init_var_from_computed_lvalue
// CHECK:   [[GETTER:%.*]] = function_ref @_TF21copy_lvalue_peepholesg8computedBi64_
// CHECK:   [[GOTTEN:%.*]] = apply [[GETTER]]()
// CHECK:   store [[GOTTEN]] to {{%.*}}
func init_var_from_computed_lvalue() {
  var y = computed
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes27assign_computed_from_lvalue
// CHECK:   [[Y:%.*]] = alloc_box
// CHECK:   [[PBY:%.*]] = project_box [[Y]]
// CHECK:   [[Y_VAL:%.*]] = load [[PBY]]
// CHECK:   [[SETTER:%.*]] = function_ref @_TF21copy_lvalue_peepholess8computedBi64_
// CHECK:   apply [[SETTER]]([[Y_VAL]])
func assign_computed_from_lvalue(y: Int) {
  var y = y
  computed = y
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes24assign_var_from_computed
// CHECK:   [[X:%.*]] = alloc_box
// CHECK:   [[PBX:%.*]] = project_box [[X]]
// CHECK:   assign {{%.*}} to [[PBX]]
func assign_var_from_computed(x: inout Int) {
  x = computed
}
