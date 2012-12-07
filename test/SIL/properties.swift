// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

/*
TODO MemberRefExpr
struct Val {
  var x, y : Int
}

class Ref {
  var x, y : Int
}

func physical_struct_lvalue(v:Val, a:Int) {
  v.y = a
}

func physical_class_lvalue(r:Ref, a:Int) {
  r.y = a
}
*/

// CHECK: func_decl physical_tuple_lvalue
func physical_tuple_lvalue(a:Int, b:Int, c:Int) {
  // CHECK: [[ABOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[BBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[CBOX:%[0-9]+]] = alloc_box $Int
  var x = (a, b)
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $(Int, Int)
  x.$1 = c
  // CHECK: [[C:%[0-9]+]] = load [[CBOX]]#1
  // CHECK: [[X_1:%[0-9]+]] = element_addr [[XBOX]]#1, 1
  // CHECK: store [[C]] to [[X_1]]
}

/* BLOCKED: rdar://12830375
func physical_tuple_rvalue(a:Int, b:Int) -> Int {
  return (a, b).$1
}
*/

/*
TODO logical properties

func take_int(x:Int)
func give_int() -> Int

var global_value_property:Int {
  get { return 1 }
  set { println("set x") }
}

func use_global_value_property() {
  take_int(global_value_property)
  global_value_property = give_int()
}
*/
