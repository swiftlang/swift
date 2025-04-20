// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil | %FileCheck %s

//--- test.h
#include <stdbool.h>

#define MACRO_INT 42

const int const_int = 42;
static int static_int = 42;
static const int static_const_int = 42;

static const bool static_const_bool = true;
static const char static_const_char = 42;
static const long static_const_long = 42;
static const float static_const_float = 42.0;
static const double static_const_double = 42.0;

//--- main.swift
func foo() {
  print(MACRO_INT)

  print(const_int)
  print(static_int)
  print(static_const_int)

  print(static_const_bool)
  print(static_const_char)
  print(static_const_long)
  print(static_const_float)
  print(static_const_double)
}

// Only 'static' cannot get the getter imported, because it's not guaranteed to be constant.
// CHECK: sil_global public_external @static_int : $Int32

// CHECK:      sil shared [transparent] @$sSC9MACRO_INTs5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo9const_ints5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo16static_const_ints5Int32Vvg : $@convention(thin) () -> Int32 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %1 = struct $Int32 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo17static_const_boolSbvg : $@convention(thin) () -> Bool {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int1, -1
// CHECK-NEXT:   %1 = struct $Bool (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo17static_const_chars4Int8Vvg : $@convention(thin) () -> Int8 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int8, 42
// CHECK-NEXT:   %1 = struct $Int8 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo17static_const_longSivg : $@convention(thin) () -> Int {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int64, 42
// CHECK-NEXT:   %1 = struct $Int (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo18static_const_floatSfvg : $@convention(thin) () -> Float {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = float_literal $Builtin.FPIEEE32, 0x42280000 // 42
// CHECK-NEXT:   %1 = struct $Float (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo19static_const_doubleSdvg : $@convention(thin) () -> Double {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = float_literal $Builtin.FPIEEE64, 0x4045000000000000 // 42
// CHECK-NEXT:   %1 = struct $Double (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }
