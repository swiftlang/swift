// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | FileCheck %s

typealias Int = Builtin.Int64

var zero = getInt()
func getInt() -> Int { return zero }

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes20init_var_from_lvalue
// CHECK:   [[X:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[Y:%.*]] = alloc_box $Builtin.Int64
// CHECK:   copy_addr [[X]]#1 to [initialization] [[Y]]#1 : $*Builtin.Int64
func init_var_from_lvalue(var x: Int) {
  var y = x
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes22assign_var_from_lvalue
// CHECK:   [[X:%.*]] = alloc_box $Builtin.Int64
// CHECK:   [[Y:%.*]] = alloc_box $Builtin.Int64
// CHECK:   copy_addr [[Y]]#1 to [[X]]#1
func assign_var_from_lvalue(inout x: Int, var y: Int) {
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
// CHECK:   [[Y_VAL:%.*]] = load [[Y]]#1
// CHECK:   [[SETTER:%.*]] = function_ref @_TF21copy_lvalue_peepholess8computedBi64_
// CHECK:   apply [[SETTER]]([[Y_VAL]])
func assign_computed_from_lvalue(var y: Int) {
  computed = y
}

// CHECK-LABEL: sil hidden @_TF21copy_lvalue_peepholes24assign_var_from_computed
// CHECK:   [[X:%.*]] = alloc_box
// CHECK:   assign {{%.*}} to [[X]]#1
func assign_var_from_computed(inout x: Int) {
  x = computed
}
