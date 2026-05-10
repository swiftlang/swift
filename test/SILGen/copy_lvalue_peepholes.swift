// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-stdlib -parse-as-library %s | %FileCheck %s

precedencegroup AssignmentPrecedence { assignment: true }

typealias Int = Builtin.Int64

var zero = getInt()
func getInt() -> Int { return zero }

// CHECK-LABEL: sil hidden [ossa] @$s21copy_lvalue_peepholes014init_var_from_B0{{[_0-9a-zA-Z]*}}F
// CHECK:   [[X:%.*]] = alloc_box ${ var Builtin.Int64 }
// CHECK:   [[XLIFE:%.*]] = begin_borrow [var_decl] [[X]]
// CHECK:   [[PBX:%.*]] = project_box [[XLIFE]]
// CHECK:   [[Y:%.*]] = alloc_box ${ var Builtin.Int64 }
// CHECK:   [[YLIFE:%.*]] = begin_borrow [var_decl] [[Y]]
// CHECK:   [[PBY:%.*]] = project_box [[YLIFE]]
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBX]]
// CHECK:   copy_addr [[READ]] to [init] [[PBY]] : $*Builtin.Int64
func init_var_from_lvalue(x: Int) {
  var x = x
  var y = x
}

// -- Peephole doesn't apply to computed lvalues

var computed: Int {
  get {
    return zero
  }
  set {}
}

// CHECK-LABEL: sil hidden [ossa] @$s21copy_lvalue_peepholes023init_var_from_computed_B0{{[_0-9a-zA-Z]*}}F
// CHECK:   [[GETTER:%.*]] = function_ref @$s21copy_lvalue_peepholes8computedBi64_vg
// CHECK:   [[GOTTEN:%.*]] = apply [[GETTER]]()
// CHECK:   store [[GOTTEN]] to [trivial] {{%.*}}
func init_var_from_computed_lvalue() {
  var y = computed
}

// CHECK-LABEL: sil hidden [ossa] @$s21copy_lvalue_peepholes021assign_computed_from_B0{{[_0-9a-zA-Z]*}}F
// CHECK:   [[Y:%.*]] = alloc_box
// CHECK:   [[YLIFE:%.*]] = begin_borrow [var_decl] [[Y]]
// CHECK:   [[PBY:%.*]] = project_box [[YLIFE]]
// CHECK:   [[READ:%.*]] = begin_access [read] [unknown] [[PBY]]
// CHECK:   [[Y_VAL:%.*]] = load [trivial] [[READ]]
// CHECK:   [[SETTER:%.*]] = function_ref @$s21copy_lvalue_peepholes8computedBi64_vs
// CHECK:   apply [[SETTER]]([[Y_VAL]])
func assign_computed_from_lvalue(y: Int) {
  var y = y
  computed = y
}

// CHECK-LABEL: sil hidden [ossa] @$s21copy_lvalue_peepholes24assign_var_from_computed{{[_0-9a-zA-Z]*}}F
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] %0
// CHECK:   assign {{%.*}} to [[WRITE]]
func assign_var_from_computed(x: inout Int) {
  x = computed
}
