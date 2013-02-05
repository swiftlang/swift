// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s
// RUN: %swift -constraint-checker -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s

import ctypes

// CHECK: define void @_T9ctypes_ir9testColorFT_T_
func testColor() {
  // CHECK: call i32 @_T5greenV5Colorg()
  var c : Color = green
}

// CHECK: define linkonce_odr hidden i32 @_T5greenV5Colorg
// CHECK: call i32 @_TV5ColorCfMS_FT5valueVSs6UInt32_S_(i32 1)

// CHECK: efine void @_T9ctypes_ir12testAnonEnumFT_T_
func testAnonEnum() {
     // CHECK: call i32 @_T10AnonConst2VSs6UInt32g
  var a = AnonConst2
}

// CHECK: define linkonce_odr hidden i32 @_T10AnonConst2VSs6UInt32g
// CHECK-NOT: ret i32
// CHECK: store i32 8
// CHECK: ret i32
