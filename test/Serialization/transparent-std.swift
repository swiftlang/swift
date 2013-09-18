// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -parse-stdlib -o %t %S/Inputs/def_transparent_std.swift
// RUN: llvm-bcanalyzer %t/def_transparent_std.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -parse-stdlib -I=%t %s | FileCheck %s -check-prefix=SIL

// CHECK-NOT: UnknownCode

import def_transparent_std

// SIL-LABEL: sil @_T19def_transparent_std23class_to_object_pointerFT1cCS_1C_Bo : $[thin] (c : C) -> Builtin.ObjectPointer {
// SIL: bb0(%0 : $C):
// SIL: %1 = alloc_stack $C
// SIL: store %0 to %1#1 : $*C
// SIL: %3 = module #Builtin
// SIL: strong_retain %0 : $C
// SIL: %5 = ref_to_object_pointer %0 : $C to $Builtin.ObjectPointer
// SIL: strong_release %0 : $C
// SIL: dealloc_stack %1#0 : $*[local_storage] C

// SIL-LABEL: sil @_T19def_transparent_std25class_from_object_pointerFT1pBo_CS_1C : $[thin] (p : Builtin.ObjectPointer) -> C {
// SIL: object_pointer_to_ref %0 : $Builtin.ObjectPointer to $C

// SIL-LABEL: sil @_T19def_transparent_std20class_to_raw_pointerFT1cCS_1C_Bp : $[thin] (c : C) -> Builtin.RawPointer {
// SIL: ref_to_raw_pointer %0 : $C to $Builtin.RawPointer

// SIL-LABEL: sil @_T19def_transparent_std22class_from_raw_pointerFT1pBp_CS_1C : $[thin] (p : Builtin.RawPointer) -> C {
// SIL: raw_pointer_to_ref %0 : $Builtin.RawPointer to $C

// SIL-LABEL: sil @_T19def_transparent_std5gep32FT1pBp1iBi32__Bp : $[thin] (p : Builtin.RawPointer, i : Builtin.Int32) -> Builtin.RawPointer {
// SIL: index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Int32

// SIL-LABEL: sil @_T19def_transparent_std11destroy_objFT1xBp_T_ : $[thin] (x : Builtin.RawPointer) -> () {
// SIL: pointer_to_address %0 : $Builtin.RawPointer to $*Builtin.ObjectPointer
// SIL: destroy_addr %6 : $*Builtin.ObjectPointer

// SIL-LABEL: sil @_T19def_transparent_std12assign_tupleFT1xTBi64_Bo_1yBp_T_ : $[thin] (x : (Builtin.Int64, Builtin.ObjectPointer), y : Builtin.RawPointer) -> () {
// SIL: %4 = tuple_element_addr %3#1 : $*(Builtin.Int64, Builtin.ObjectPointer), 0
// SIL: %5 = tuple_element_addr %3#1 : $*(Builtin.Int64, Builtin.ObjectPointer), 1
// SIL: %7 = tuple (%0 : $Builtin.Int64, %1 : $Builtin.ObjectPointer)
// SIL: %12 = tuple (%0 : $Builtin.Int64, %1 : $Builtin.ObjectPointer)
// SIL: %13 = tuple_extract %12 : $(Builtin.Int64, Builtin.ObjectPointer), 1
// SIL: %15 = tuple_extract %12 : $(Builtin.Int64, Builtin.ObjectPointer), 0
// SIL: %16 = tuple_extract %12 : $(Builtin.Int64, Builtin.ObjectPointer), 1
// SIL: %17 = pointer_to_address %2 : $Builtin.RawPointer to $*(Builtin.Int64, Builtin.ObjectPointer)
// SIL: %18 = tuple (%15 : $Builtin.Int64, %16 : $Builtin.ObjectPointer)
// SIL: %19 = load %17 : $*(Builtin.Int64, Builtin.ObjectPointer)

// SIL-LABEL: sil @_T19def_transparent_std3fooFT1xBi1_1yBi1__Bi1_ : $[thin] (x : Builtin.Int1, y : Builtin.Int1) -> Builtin.Int1 {
// SIL: builtin_function_ref #Builtin.cmp_eq_Int1 : $[thin] (Builtin.Int1, Builtin.Int1) -> Builtin.Int1
// SIL: %8 = apply %7(%0, %1) : $[thin] (Builtin.Int1, Builtin.Int1) -> Builtin.Int1
func test_foo(x:Builtin.Int1, y:Builtin.Int1) -> Builtin.Int1 {
  var a = foo(x, y)
  return a
}

func test_tuple(x:(Builtin.Int64, Builtin.ObjectPointer),
                y:Builtin.RawPointer) {
  assign_tuple(x, y)
}

func test_conversion(c:C, t32:Builtin.Int32) {
  var b = class_to_object_pointer(c)
  var c = class_from_object_pointer(b)
  var d = class_to_raw_pointer(c)
  var e = class_from_raw_pointer(d)
  var f = gep32(d, t32)
  destroy_obj(d)
}
