// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Extern %t/extern_c.swift -I%t | %FileCheck %s --check-prefixes CHECK,CHECK-%target-cpu

// REQUIRES: swift_feature_Extern

//--- c_abi_types.h
#include <stdbool.h>

void c_roundtrip_void(void);
bool c_roundtrip_bool(bool);
char c_roundtrip_char(char);
int c_roundtrip_int(int);
float c_roundtrip_float(float);
double c_roundtrip_double(double);
struct incomplete_ty;
struct incomplete_ty *c_roundtrip_opaque_ptr(struct incomplete_ty *);
void *c_roundtrip_void_star(void *);

struct c_struct {
  int foo;
  long bar;
  char baz;
  // with tail pad
};
struct c_struct c_roundtrip_c_struct(struct c_struct);
struct c_struct *c_roundtrip_c_struct_ptr(struct c_struct*);

//--- module.modulemap

module c_abi_types {
  header "c_abi_types.h"
}

//--- extern_c.swift

import c_abi_types

@_extern(c, "swift_roundtrip_void")
func swift_roundtrip_void()
@_extern(c, "swift_roundtrip_bool")
func swift_roundtrip_bool(_: Bool) -> Bool
@_extern(c, "swift_roundtrip_char")
func swift_roundtrip_char(_: CChar) -> CChar
@_extern(c, "swift_roundtrip_int")
func swift_roundtrip_int(_: CInt) -> CInt
@_extern(c, "swift_roundtrip_float")
func swift_roundtrip_float(_: CFloat) -> CFloat
@_extern(c, "swift_roundtrip_double")
func swift_roundtrip_double(_: CDouble) -> CDouble
@_extern(c, "swift_roundtrip_opaque_ptr")
func swift_roundtrip_opaque_ptr(_: OpaquePointer!) -> OpaquePointer!
@_extern(c, "swift_roundtrip_void_star")
func swift_roundtrip_void_star(_: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer!
@_extern(c, "swift_roundtrip_c_struct")
func swift_roundtrip_c_struct(_: c_struct) -> c_struct
@_extern(c, "swift_roundtrip_c_struct_ptr")
func swift_roundtrip_c_struct_ptr(_: UnsafeMutablePointer<c_struct>!) -> UnsafeMutablePointer<c_struct>!

func test() {
  // CHECK: call void @c_roundtrip_void()
  // CHECK: call void @swift_roundtrip_void()
  c_roundtrip_void()
  swift_roundtrip_void()

  // CHECK: call [[BOOL_TYPE:.+]]  @c_roundtrip_bool([[BOOL_TRUE:.+]])
  // CHECK: call [[BOOL_TYPE]] @swift_roundtrip_bool([[BOOL_TRUE]])
  _ = c_roundtrip_bool(true)
  _ = swift_roundtrip_bool(true)

  // CHECK: call [[CCHAR_TYPE:.+]]  @c_roundtrip_char([[CCHAR_VALUE:.+]])
  // CHECK: call [[CCHAR_TYPE]] @swift_roundtrip_char([[CCHAR_VALUE]])
  _ = c_roundtrip_char(0x61)
  _ = swift_roundtrip_char(0x61)

  // CHECK: call [[CINT_TYPE:.+]]  @c_roundtrip_int([[CINT_VALUE:.+]])
  // CHECK: call [[CINT_TYPE]] @swift_roundtrip_int([[CINT_VALUE]])
  _ = c_roundtrip_int(42)
  _ = swift_roundtrip_int(42)

  // CHECK: call [[CFLOAT_TYPE:.+]]  @c_roundtrip_float([[CFLOAT_VALUE:.+]])
  // CHECK: call [[CFLOAT_TYPE]] @swift_roundtrip_float([[CFLOAT_VALUE]])
  _ = c_roundtrip_float(3.14)
  _ = swift_roundtrip_float(3.14)

  // CHECK: call [[CDOUBLE_TYPE:.+]]  @c_roundtrip_double([[CDOUBLE_VALUE:.+]])
  // CHECK: call [[CDOUBLE_TYPE]] @swift_roundtrip_double([[CDOUBLE_VALUE]])
  _ = c_roundtrip_double(3.1415)
  _ = swift_roundtrip_double(3.1415)

  // CHECK: call [[OPAQUE_PTR_TYPE:.+]]  @c_roundtrip_opaque_ptr([[OPAQUE_PTR_VALUE:.+]])
  // CHECK: call [[OPAQUE_PTR_TYPE]] @swift_roundtrip_opaque_ptr([[OPAQUE_PTR_VALUE]])
  _ = c_roundtrip_opaque_ptr(nil)
  _ = swift_roundtrip_opaque_ptr(nil)

  // CHECK: call [[VOID_STAR_TYPE:.+]]  @c_roundtrip_void_star([[VOID_STAR_TYPE]] {{.*}})
  // CHECK: call [[VOID_STAR_TYPE]] @swift_roundtrip_void_star([[VOID_STAR_TYPE]] {{.*}})
  let nonNullPtr = UnsafeMutableRawPointer(bitPattern: 0xbeef)!
  _ = c_roundtrip_void_star(nonNullPtr)
  _ = swift_roundtrip_void_star(nonNullPtr)

  // assume %struct.c_struct and %TSo8c_structV have compatible layout
  //
  // CHECK-x86_64: call void     @c_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}} {{.*}}, ptr{{( byval\(%struct.c_struct\))?}}[[ALIGN:(align [0-9]+)?]] {{.*}})
  // CHECK-x86_64: call void @swift_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}}   {{.*}}, ptr{{( byval\(%TSo8c_structV\))?}}[[ALIGN]] {{.*}})
  // CHECK-arm64:  call void     @c_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}} {{.*}}, ptr{{( byval\(%struct.c_struct\))?}}[[ALIGN:(align [0-9]+)?]] {{.*}})
  // CHECK-arm64:  call void @swift_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}}   {{.*}}, ptr{{( byval\(%TSo8c_structV\))?}}[[ALIGN]] {{.*}})
  // CHECK-wasm32: call void     @c_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}} {{.*}}, ptr{{( byval\(%struct.c_struct\))?}}[[ALIGN:(align [0-9]+)?]] {{.*}})
  // CHECK-wasm32: call void @swift_roundtrip_c_struct(ptr noalias{{( nocapture)?}} sret(%TSo8c_structV){{( captures\(none\))?}}   {{.*}}, ptr{{( byval\(%TSo8c_structV\))?}}[[ALIGN]] {{.*}})
  // CHECK-armv7k: call [3 x i32]     @c_roundtrip_c_struct([3 x i32] {{.*}})
  // CHECK-armv7k: call [3 x i32] @swift_roundtrip_c_struct([3 x i32] {{.*}})
  var c_struct_val = c_struct(foo: 496, bar: 28, baz: 8)
  _ = c_roundtrip_c_struct(c_struct_val)
  _ = swift_roundtrip_c_struct(c_struct_val)

  withUnsafeMutablePointer(to: &c_struct_val) { c_struct_ptr in
    // CHECK: call [[C_STRUCT_PTR_TYPE:.+]]  @c_roundtrip_c_struct_ptr([[C_STRUCT_PTR_TYPE]] {{.*}})
    // CHECK: call [[C_STRUCT_PTR_TYPE]] @swift_roundtrip_c_struct_ptr([[C_STRUCT_PTR_TYPE]] {{.*}})
    _ = c_roundtrip_c_struct_ptr(c_struct_ptr)
    _ = swift_roundtrip_c_struct_ptr(c_struct_ptr)
  }
}

test()
