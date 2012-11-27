// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -emit-llvm -o - %s | FileCheck %s

import objc

// CHECK: @"\01L_OBJC_METH_VAR_NAME_" = internal constant [18 x i8] c"method:withFloat:\00"
// CHECK: @"\01L_OBJC_METH_VAR_NAME_1" = internal constant [19 x i8] c"method:withDouble:\00"

// Instance method invocation
// CHECK: define void @_T7objc_ir15instanceMethodsFT1bC1B_T_(%_T1B*
func instanceMethods(b : B) {
  // CHECK: load i8** @"selector(method:withFloat:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  var i = b.method(1, withFloat=2.5)
  // CHECK: load i8** @"selector(method:withDouble:)"
  // CHECK: call i32 bitcast (void ()* @objc_msgSend to i32
  i = i + b.method(1, withDouble=2.5)
}

// FIXME: Should run, but currently doesn't.
println(Int(swift_createDate().isEqualToDate(swift_createDate())))

