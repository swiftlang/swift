// RUN: %swift -dump-sil %s | FileCheck %s

// TODO: var_decl $Int, @x
var x : Int

// CHECK: func_decl print_x
func print_x() {
  println(x)
}

x = 0
print_x()

// TODO: var_decl $Int, @y
var y : Int

// CHECK: func_decl print_y
func print_y() {
  println(y)
}

y = 1
print_y()

// CHECK: toplevel
// CHECK: bb0:
// CHECK: [[X:%[0-9]+]] = constant_ref {{.*}} @x
// CHECK: store {{.*}} to [[X]]
// CHECK: [[PRINT_X:%[0-9]+]] = constant_ref {{.*}} @print_x
// CHECK: apply [[PRINT_X]]
// CHECK: [[Y:%[0-9]+]] = constant_ref {{.*}} @y
// CHECK: store {{.*}} to [[Y]]
// CHECK: [[PRINT_Y:%[0-9]+]] = constant_ref {{.*}} @print_y
// CHECK: apply [[PRINT_Y]]
// CHECK: [[RET:%[0-9]+]] = tuple ()
// CHECK: return ([[RET]])

