// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Extern %s | %FileCheck %s

// REQUIRES: swift_feature_Extern

func test() {
  // CHECK: call void @explicit_extern_c()
  explicit_extern_c()
  // CHECK: call void @implicit_extern_c()
  implicit_extern_c()
  // CHECK: [[DEFAULT_ARG:%[0-9]+]] = call swiftcc i32 @"$s8extern_c17default_arg_valueyys5Int32VFfA_"()
  // CHECK: call void @default_arg_value(i32 [[DEFAULT_ARG]])
  default_arg_value()
  // CHECK: call void @default_arg_value(i32 24)
  default_arg_value(24)
}

test()

// CHECK: declare void @explicit_extern_c()
@_extern(c, "explicit_extern_c") func explicit_extern_c()

// CHECK: declare void @implicit_extern_c()
@_extern(c) func implicit_extern_c()

// CHECK: declare void @default_arg_value(i32)
@_extern(c) func default_arg_value(_: Int32 = 42)
