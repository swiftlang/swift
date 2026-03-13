// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Extern %s | %FileCheck %s

// REQUIRES: swift_feature_Extern


// CHECK: @pointer_c = external global
@_extern(c)
var pointer_c: UnsafeMutablePointer<Int>

// CHECK: @nullable_pointer_c = external global
@_extern(c)
var nullable_pointer_c: UnsafeMutablePointer<Int>?

func acceptInt(_: Int) { }

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

  // CHECK: call void @swift_beginAccess(ptr @pointer_c
  // CHECK-NEXT: [[POINTEE_VAL:%[0-9]+]] = load ptr, ptr @pointer_c
  // CHECK-NEXT: call void @swift_endAccess
  acceptInt(pointer_c.pointee)
}

test()

// CHECK: declare void @explicit_extern_c()
@_extern(c, "explicit_extern_c") func explicit_extern_c()

// CHECK: declare void @implicit_extern_c()
@_extern(c) func implicit_extern_c()

// CHECK: declare void @default_arg_value(i32)
@_extern(c) func default_arg_value(_: Int32 = 42)
