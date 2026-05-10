// RUN: %empty-directory(%t)

// RUN: cat %S/small-structs-pass-return-direct-in-c.swift %S/small-structs-64-bit-pass-return-direct-in-c.swift > %t/full-small-structs-pass-return-direct-in-c.swift

// RUN: %target-swift-frontend %t/full-small-structs-pass-return-direct-in-c.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clang -c %s -I %t -o %t/swift-structs-execution.o -Wno-incompatible-pointer-types
// RUN: %target-interop-build-swift %t/full-small-structs-pass-return-direct-in-c.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

// FIXME: can we also make a generic test?
// REQUIRES: PTRSIZE=64

#include <assert.h>
#include "structs.h"

int main() {
  // printStructOneI64(returnNewStructOneI64())
  struct swift_interop_returnStub_Structs_uint64_t_0_8 structOneI64;
  swift_interop_returnDirect_Structs_uint64_t_0_8(&structOneI64, $s7Structs21returnNewStructOneI64AA0deF0VyF());
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_uint64_t_0_8(&structOneI64));
// CHECK:      StructOneI64.x = 42
  // printStructOneI64(passThroughStructOneI64(...))
  struct swift_interop_returnStub_Structs_uint64_t_0_8 structOneI64_copy;
  swift_interop_returnDirect_Structs_uint64_t_0_8(&structOneI64_copy,
    $s7Structs23passThroughStructOneI64yAA0deF0VADF(swift_interop_passDirect_Structs_uint64_t_0_8(&structOneI64)));
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_uint64_t_0_8(&structOneI64_copy));
// CHECK-NEXT: StructOneI64.x = 42
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_uint64_t_0_8(&structOneI64));
// CHECK-NEXT: StructOneI64.x = 42
  // printStructTwoI32(returnNewStructTwoI32(11))
  struct swift_interop_returnStub_Structs_uint64_t_0_8 structTwoI32;
  swift_interop_returnDirect_Structs_uint64_t_0_8(&structTwoI32,
                                                  $s7Structs21returnNewStructTwoI32yAA0deF0Vs5Int32VF(11));
  $s7Structs17printStructTwoI32yyAA0cdE0VF(swift_interop_passDirect_Structs_uint64_t_0_8(&structTwoI32));
// CHECK-NEXT: StructTwoI32.x = 11, y = 22
  // printStructTwoI32(passThroughStructTwoI32(4, ..., 6))
  struct swift_interop_returnStub_Structs_uint64_t_0_8 structTwoI32_copy;
  swift_interop_returnDirect_Structs_uint64_t_0_8(&structTwoI32_copy,
    $s7Structs23passThroughStructTwoI32yAA0deF0Vs5Int32V_AdFtF(4, swift_interop_passDirect_Structs_uint64_t_0_8(&structTwoI32), 6));
  $s7Structs17printStructTwoI32yyAA0cdE0VF(swift_interop_passDirect_Structs_uint64_t_0_8(&structTwoI32_copy));
// CHECK-NEXT: StructTwoI32.x = 15, y = 28
  // printStructStructTwoI32_and_OneI16AndOneStruct(... , returnNewStructOneI16AndOneStruct());
  struct swift_interop_returnStub_Structs_uint64_t_0_8_uint32_t_8_12 structOneI16AndOneStruct;
  swift_interop_returnDirect_Structs_uint64_t_0_8_uint32_t_8_12(&structOneI16AndOneStruct,
    $s7Structs024returnNewStructOneI16AndeD0AA0defgeD0VyF());
  $s7Structs011printStructc20TwoI32_and_OneI16AndgC0yyAA0cdE0V_AA0cghigC0VtF(
    swift_interop_passDirect_Structs_uint64_t_0_8(&structTwoI32),
    swift_interop_passDirect_Structs_uint64_t_0_8_uint32_t_8_12(&structOneI16AndOneStruct));
// CHECK-NEXT: StructTwoI32.x = 11, y = 22
// CHECK-NEXT: StructOneI16AndOneStruct.x = 255, y.x = 5, y.y = 72
  // let x = returnNewStructU16AndPointer(...)
  // getStructU16AndPointer_x(x)
  // getStructU16AndPointer_y(y)
  char c = 'A';
  struct swift_interop_returnStub_Structs_uint8_t_0_1_void_ptr_8_16 structU16AndPointer;
  swift_interop_returnDirect_Structs_uint8_t_0_1_void_ptr_8_16(&structU16AndPointer,
    $s7Structs28returnNewStructU16AndPointeryAA0defG0VSvF(&c));
  assert($s7Structs24getStructU16AndPointer_xys5UInt8VAA0cdeF0VF(
    swift_interop_passDirect_Structs_uint8_t_0_1_void_ptr_8_16(&structU16AndPointer)) == 55);
  assert($s7Structs24getStructU16AndPointer_yySvAA0cdeF0VF(
    swift_interop_passDirect_Structs_uint8_t_0_1_void_ptr_8_16(&structU16AndPointer)) == &c);

  // let x = returnNewStructDoubleAndFloat()
  // getStructDoubleAndFloat_x(x)
  // getStructDoubleAndFloat_y(x)
  double doubleValue = 1.25;
  float floatValue = -5.0f;
  struct swift_interop_returnStub_Structs_double_0_8_float_8_12 structDoubleAndFloat;
  swift_interop_returnDirect_Structs_double_0_8_float_8_12(&structDoubleAndFloat,
    $s7Structs29returnNewStructDoubleAndFloatyAA0defG0VSf_SdtF(floatValue, doubleValue));
  assert($s7Structs25getStructDoubleAndFloat_xySdAA0cdeF0VF(
    swift_interop_passDirect_Structs_double_0_8_float_8_12(&structDoubleAndFloat)) == doubleValue);
  assert($s7Structs25getStructDoubleAndFloat_yySfAA0cdeF0VF(
    swift_interop_passDirect_Structs_double_0_8_float_8_12(&structDoubleAndFloat)) == floatValue);

  // printStructI8AndU32AndI16(returnNewStructI8AndU32AndI16())
  struct swift_interop_returnStub_Structs_uint64_t_0_8_uint16_t_8_10 structI8AndU32AndI16;
  swift_interop_returnDirect_Structs_uint64_t_0_8_uint16_t_8_10(&structI8AndU32AndI16,
    $s7Structs023returnNewStructI8AndU32F3I16AA0defgfH0VyF());
  $s7Structs019printStructI8AndU32E3I16yyAA0cdefeG0VF(
    swift_interop_passDirect_Structs_uint64_t_0_8_uint16_t_8_10(&structI8AndU32AndI16));
// CHECK-NEXT: StructI8AndU32AndI16.x = -100, y = 123456, z = -3456
  return 0;
}
