// RUN: %swift %s -emit-ir -target arm64e-apple-ios13.0 -I %S/Inputs/ -validate-tbd-against-ir=none -Xllvm -sil-disable-pass=OnoneSimplification | %FileCheck %s
// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// A __ptrauth-qualified data pointer is authenticated when it is read and
// signed when it is written. Unlike a C function pointer, it is not re-signed
// with the function pointer schema.

import PointerAuthDataPointers

// CHECK-LABEL: define {{.*}} @"$s29ptrauth_field_data_ptr_import05test_c1_D5_readSvSgyF"()
// CHECK: resign-nonnull:
// CHECK: call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 77)
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: ret
func test_data_ptr_read() -> UnsafeMutableRawPointer? {
  return ptr_to_data_pointer_struct!.pointee.data_ptr
}

// CHECK-LABEL: define {{.*}} @"$s29ptrauth_field_data_ptr_import024test_addr_discriminated_D5_readSvSgyF"()
// CHECK: resign-nonnull:
// CHECK: [[DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 77)
// CHECK: call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 [[DISC]])
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: ret
func test_addr_discriminated_ptr_read() -> UnsafeMutableRawPointer? {
  return ptr_to_addr_discriminated_data_pointer_struct!.pointee.data_ptr
}

// CHECK-LABEL: define {{.*}} @"$s29ptrauth_field_data_ptr_import024test_addr_discriminated_D6_writeyySvF"(ptr %0)
// CHECK: resign-nonnull:
// CHECK: [[DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 {{%.*}}, i64 77)
// CHECK: call i64 @llvm.ptrauth.sign(i64 {{%.*}}, i32 2, i64 [[DISC]])
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: ret
func test_addr_discriminated_ptr_write(_ p: UnsafeMutableRawPointer) {
  ptr_to_addr_discriminated_data_pointer_struct!.pointee.data_ptr = p
}

// CHECK-LABEL: define {{.*}} @"$s29ptrauth_field_data_ptr_import013test_nonnull_D5_readSvyF"()
// CHECK: resign-nonnull:
// CHECK: call i64 @llvm.ptrauth.auth(i64 {{%.*}}, i32 2, i64 {{%.*}})
// CHECK-NOT: @llvm.ptrauth.resign
// CHECK: ret
func test_nonnull_ptr_read() -> UnsafeMutableRawPointer {
  return ptr_to_nonnull_pointer_struct!.pointee.data_ptr
}

// CHECK-LABEL: define {{.*}} @"$s29ptrauth_field_data_ptr_import20test_nonnull_fn_reads5Int32VyF"()
// CHECK: resign-nonnull:
// CHECK: call i64 @llvm.ptrauth.resign(i64 {{%.*}}, i32 1, i64 {{%.*}}, i32 0, i64 0)
// CHECK: ret
func test_nonnull_fn_read() -> Int32 {
  let fn = ptr_to_nonnull_pointer_struct!.pointee.func_ptr
  return fn()
}
