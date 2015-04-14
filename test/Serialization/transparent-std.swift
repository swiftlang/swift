// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t %S/Inputs/def_transparent_std.swift
// RUN: llvm-bcanalyzer %t/def_transparent_std.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -sil-disable-pass="External Defs To Decls" -sil-debug-serialization -parse-stdlib -I %t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent_std

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std3fooFT1xBi1_1yBi1__Bi1_ : $@convention(thin) (Builtin.Int1, Builtin.Int1) -> Builtin.Int1 {
// SIL: = builtin "cmp_eq_Int1"(%0 : $Builtin.Int1, %1 : $Builtin.Int1) : $Builtin.Int1
func test_foo(x: Builtin.Int1, y: Builtin.Int1) -> Builtin.Int1 {
  var a = foo(x: x, y: y)
  return a
}

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std12assign_tupleFT1xTBi64_Bo_1yBp_T_ : $@convention(thin) (Builtin.Int64, @owned Builtin.NativeObject, Builtin.RawPointer) -> () {
// SIL: = tuple (%0 : $Builtin.Int64, %1 : $Builtin.NativeObject)
// SIL: retain_value
// SIL: = tuple_extract
// SIL: = tuple_extract
// SIL: = pointer_to_address
// SIL: = tuple
// SIL: = load
func test_tuple(x: (Builtin.Int64, Builtin.NativeObject),
                y: Builtin.RawPointer) {
  assign_tuple(x: x, y: y)
}

func test_conversion(c: C, t32: Builtin.Int32) {
// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std22class_to_native_objectFT1cCS_1C_Bo : $@convention(thin) (@owned C) -> @owned Builtin.NativeObject {
// SIL: bb0(%0 : $C):
// SIL: unchecked_ref_cast %0 : $C to $Builtin.NativeObject
// SIL-NEXT: return
  var b = class_to_native_object(c: c)

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std24class_from_native_objectFT1pBo_CS_1C : $@convention(thin) (@owned Builtin.NativeObject) -> @owned C {
// SIL: unchecked_ref_cast %0 : $Builtin.NativeObject to $C
  var c = class_from_native_object(p: b)

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std20class_to_raw_pointerFT1cCS_1C_Bp : $@convention(thin) (@owned C) -> Builtin.RawPointer {
// SIL: ref_to_raw_pointer %0 : $C to $Builtin.RawPointer
  var d = class_to_raw_pointer(c: c)

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std22class_from_raw_pointerFT1pBp_CS_1C : $@convention(thin) (Builtin.RawPointer) -> @owned C {
// SIL: raw_pointer_to_ref %0 : $Builtin.RawPointer to $C
  var e = class_from_raw_pointer(p: d)

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std5gep32FT1pBp1iBi32__Bp : $@convention(thin) (Builtin.RawPointer, Builtin.Int32) -> Builtin.RawPointer {
// SIL: index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Int32
  var f = gep32(p: d, i: t32)

// SIL-LABEL: sil public_external [transparent] [fragile] @_TF19def_transparent_std11destroy_objFT1xBp_T_ : $@convention(thin) (Builtin.RawPointer) -> () {
// SIL: pointer_to_address %0 : $Builtin.RawPointer to $*Builtin.NativeObject
  destroy_obj(x: d)
}
