// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil @main
// CHECK: bb0({{%.*}} : $Int32, {{%.*}} : $UnsafeMutablePointer<UnsafeMutablePointer<Int8>>):

// -- initialize x
// CHECK: [[X:%[0-9]+]] = global_addr @_Tv8toplevel1xSi : $*Int
// CHECK: integer_literal $Builtin.Int2048, 999
// CHECK: store {{.*}} to [[X]]

// -- assign x
// CHECK: integer_literal $Builtin.Int2048, 0
// CHECK: assign {{.*}} to [[X]]
// CHECK: [[PRINT_X:%[0-9]+]] = function_ref @_TF8toplevel7print_xFT_T_ :
// CHECK: apply [[PRINT_X]]

// -- assign y
// CHECK: [[Y1:%[0-9]+]] = global_addr @_Tv8toplevel1ySi : $*Int
// CHECK: [[Y:%[0-9]+]] = mark_uninitialized [var] [[Y1]]
// CHECK: assign {{.*}} to [[Y]]
// CHECK: [[PRINT_Y:%[0-9]+]] = function_ref @_TF8toplevel7print_yFT_T_
// CHECK: [[RET:%[0-9]+]] = struct $Int32
// CHECK: return [[RET]]

var x = 999

// CHECK-LABEL: sil hidden @_TF8toplevel7print_xFT_T_
func print_x() {
  println(x)
}

x = 0
print_x()

var y : Int

// CHECK-LABEL: sil hidden @_TF8toplevel7print_yFT_T_
func print_y() {
  println(y)
}

y = 1
print_y()


// CHECK: sil hidden @_TF8toplevel13testGlobalCSEFT_Si
// CHECK-NOT: global_addr
// CHECK: %0 = global_addr @_Tv8toplevel1xSi : $*Int
// CHECK-NOT: global_addr
// CHECK: return
func testGlobalCSE() -> Int {
  // We should only emit one global_addr in this function.
  return x + x
}
