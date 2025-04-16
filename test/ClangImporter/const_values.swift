// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil -serialize-diagnostics -serialize-diagnostics-path %t/test.diag | %FileCheck %s

// RUN: c-index-test -read-diagnostics %t/test.diag 2>&1 | %FileCheck --check-prefix CHECK-DIAG %t/src/test.h

//--- test.h
#include <stdbool.h>

#define MACRO_INT 42

const int const_int = 42;
static int static_int = 42;
static const int static_const_int = 42;

static const bool static_const_bool = true;
static const char static_const_char = 42;
static const char static_const_char_with_long_literal = 42ull;
static const char static_const_char_with_overflow_literal = 777ull; // CHECK-DIAG: [[@LINE]]:{{.*}}: warning: implicit conversion from 'unsigned long long' to 'char' changes value from 777 to 9
static const long static_const_long = 42;
static const float static_const_float = 42.0;
static const double static_const_double = 42.0;

static const char static_const_char_array[4] = {1, 2, 3, 4};
static const char *static_const_pointer = 0;

static const char static_const_char_referencing_other_const = 1 + static_const_char;

typedef enum MyEnum: char {
  MyEnumCase0 = 0, MyEnumCase1, MyEnumCase2
} MyEnum;
static const MyEnum static_const_enum = MyEnumCase1;

struct MyStruct {
  int field;
};
__attribute__((swift_name("MyStruct.static_const_int_as_a_member")))
static const int static_const_int_as_a_member = 42;

typedef int ImportAsStruct __attribute__((swift_wrapper(struct)));
static const ImportAsStruct ImportAsStructFoo = 123;
typedef int ImportAsEnum __attribute__((swift_wrapper(enum)));
static const ImportAsEnum ImportAsEnumFoo = 123;

//--- main.swift
func foo() {
  print(MACRO_INT)

  print(const_int)
  print(static_int)
  print(static_const_int)

  print(static_const_bool)
  print(static_const_char)
  print(static_const_char_with_long_literal)
  print(static_const_char_with_overflow_literal)
  print(static_const_long)
  print(static_const_float)
  print(static_const_double)

  print(static_const_char_array)
  print(static_const_pointer)

  print(static_const_char_referencing_other_const)

  print(static_const_enum)

  print(MyStruct.static_const_int_as_a_member)

  print(ImportAsStruct.foo)
  print(ImportAsEnum.foo)
}

// Globals that don't get their value imported stay as public_external:
// CHECK: sil_global public_external @static_int : $Int32
// CHECK: sil_global public_external [let] @static_const_char_array : $(Int8, Int8, Int8, Int8)
// CHECK: sil_global public_external @static_const_pointer : $Optional<UnsafePointer<Int8>>

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

// CHECK:      sil shared [transparent] @$sSo35static_const_char_with_long_literals4Int8Vvg : $@convention(thin) () -> Int8 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int8, 42
// CHECK-NEXT:   %1 = struct $Int8 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo39static_const_char_with_overflow_literals4Int8Vvg : $@convention(thin) () -> Int8 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int8, 9
// CHECK-NEXT:   %1 = struct $Int8 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo17static_const_long{{.*}} : $@convention(thin) () -> {{.*}} {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal {{.*}}, 42
// CHECK-NEXT:   %1 = struct {{.*}} (%0)
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

// CHECK:      sil shared [transparent] @$sSo036static_const_char_referencing_other_B0s4Int8Vvg : $@convention(thin) () -> Int8 {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int8, 43
// CHECK-NEXT:   %1 = struct $Int8 (%0)
// CHECK-NEXT:   return %1
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo17static_const_enumSo6MyEnumVvg : $@convention(thin) () -> MyEnum {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   %0 = integer_literal $Builtin.Int8, 1
// CHECK-NEXT:   %1 = struct $Int8 (%0)
// CHECK-NEXT:   %2 = struct $MyEnum (%1)
// CHECK-NEXT:   return %2
// CHECK-NEXT: }

// CHECK:      sil shared [transparent] @$sSo8MyStructV28static_const_int_as_a_members5Int32VvgZ : $@convention(method) (@thin MyStruct.Type) -> Int32 {
// CHECK-NEXT: // %0 "self"
// CHECK-NEXT: bb0(%0 : $@thin MyStruct.Type):
// CHECK-NEXT:   debug_value %0, let, name "self", argno 1
// CHECK-NEXT:   %2 = integer_literal $Builtin.Int32, 42
// CHECK-NEXT:   %3 = struct $Int32 (%2)
// CHECK-NEXT:   return %3
// CHECK-NEXT: }
