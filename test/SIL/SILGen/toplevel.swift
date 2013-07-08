// RUN: %swift -emit-sil %s | FileCheck %s

// CHECK: sil internal @top_level_code
// CHECK: bb0:

// -- initialize x
// CHECK: [[X:%[0-9]+]] = global_addr @x : $*Int64
// CHECK: integer_literal $Builtin.Int128, 999
// CHECK: store {{.*}} to [[X]]

// -- assign x
// CHECK: integer_literal $Builtin.Int128, 0
// CHECK: [[X:%[0-9]+]] = global_addr @x : $*Int64
// CHECK: store {{.*}} to [[X]]
// CHECK: [[PRINT_X:%[0-9]+]] = function_ref @_T8toplevel7print_xFT_T_ :
// CHECK: apply [[PRINT_X]]

// -- default-initialize y
// CHECK: [[Y:%[0-9]+]] = global_addr @y : $*Int64
// CHECK: [[INTCTOR:%[0-9]+]] = function_ref @_TSiCfMSiFT_Si : $[thin] ((), Int64.metatype) -> Int64
// CHECK: [[INTMETA:%[0-9]+]] = metatype $Int64.metatype
// CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCTOR]]([[INTMETA]])
// CHECK: [[INTVAL]] to [[Y]]

// -- assign y
// CHECK: [[Y:%[0-9]+]] = global_addr @y : $*Int64
// CHECK: store {{.*}} to [[Y]]
// CHECK: [[PRINT_Y:%[0-9]+]] = function_ref @_T8toplevel7print_yFT_T_
// CHECK: [[RET:%[0-9]+]] = tuple ()
// CHECK: return [[RET]]

var x = 999

// CHECK: sil @_T8toplevel7print_xFT_T_
func print_x() {
  println(x)
}

x = 0
print_x()

var y : Int

// CHECK: sil @_T8toplevel7print_yFT_T_
func print_y() {
  println(y)
}

y = 1
print_y()

