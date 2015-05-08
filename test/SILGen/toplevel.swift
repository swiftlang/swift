// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func markUsed<T>(t: T) {}

// CHECK-LABEL: sil @main
// CHECK: bb0({{%.*}} : $Int32, {{%.*}} : $UnsafeMutablePointer<UnsafeMutablePointer<Int8>>):

// -- initialize x
// CHECK: [[X:%[0-9]+]] = global_addr @_Tv8toplevel1xSi : $*Int
// CHECK: integer_literal $Builtin.Int2048, 999
// CHECK: store {{.*}} to [[X]]

var x = 999

func print_x() {
  markUsed(x)
}

// -- assign x
// CHECK: integer_literal $Builtin.Int2048, 0
// CHECK: assign {{.*}} to [[X]]
// CHECK: [[PRINT_X:%[0-9]+]] = function_ref @_TF8toplevel7print_xFT_T_ :
// CHECK: apply [[PRINT_X]]


x = 0
print_x()

// <rdar://problem/19770775> Deferred initialization of let bindings rejected at top level in playground
// CHECK: [[COUNTADDR:%[0-9]+]] = global_addr @_Tv8toplevel5countSi : $*Int
// CHECK-NEXT: [[COUNTMUI:%[0-9]+]] = mark_uninitialized [var] [[COUNTADDR]] : $*Int
let count: Int
// CHECK: cond_br
if x == 5 {
  count = 0
  // CHECK: assign {{.*}} to [[COUNTMUI]]
  // CHECK: br [[MERGE:bb[0-9]+]]
} else {
  count = 10
  // CHECK: assign {{.*}} to [[COUNTMUI]]
  // CHECK: br [[MERGE]]
}

// CHECK: [[MERGE]]:
// CHECK: load [[COUNTMUI]]
markUsed(count)



var y : Int

func print_y() {
  markUsed(y)
}


// -- assign y
// CHECK: [[Y1:%[0-9]+]] = global_addr @_Tv8toplevel1ySi : $*Int
// CHECK: [[Y:%[0-9]+]] = mark_uninitialized [var] [[Y1]]
// CHECK: assign {{.*}} to [[Y]]
// CHECK: [[PRINT_Y:%[0-9]+]] = function_ref @_TF8toplevel7print_yFT_T_
// CHECK: [[RET:%[0-9]+]] = struct $Int32
// CHECK: return [[RET]]
y = 1
print_y()

// CHECK-LABEL: sil hidden @_TF8toplevel7print_xFT_T_

// CHECK-LABEL: sil hidden @_TF8toplevel7print_yFT_T_

// CHECK: sil hidden @_TF8toplevel13testGlobalCSEFT_Si
// CHECK-NOT: global_addr
// CHECK: %0 = global_addr @_Tv8toplevel1xSi : $*Int
// CHECK-NOT: global_addr
// CHECK: return
func testGlobalCSE() -> Int {
  // We should only emit one global_addr in this function.
  return x + x
}


