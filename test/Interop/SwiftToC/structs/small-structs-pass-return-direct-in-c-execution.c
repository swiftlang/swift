// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/small-structs-pass-return-direct-in-c.swift -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clang -c %s -I %t -o %t/swift-structs-execution.o -Wno-incompatible-pointer-types
// RUN: %target-interop-build-swift %S/small-structs-pass-return-direct-in-c.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "structs.h"

int main() {
  // printStructOneI64(returnNewStructOneI64())
  struct Structs_StructOneI64 structOneI64;
  swift_interop_returnDirect_Structs_StructOneI64(&structOneI64, $s7Structs21returnNewStructOneI64AA0deF0VyF());
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_StructOneI64(&structOneI64));
// CHECK:      StructOneI64.x = 42

  // printStructOneI64(passThroughStructOneI64(...))
  struct Structs_StructOneI64 structOneI64_copy;
  swift_interop_returnDirect_Structs_StructOneI64(&structOneI64_copy,
    $s7Structs23passThroughStructOneI64yAA0deF0VADF(
      swift_interop_passDirect_Structs_StructOneI64(&structOneI64)));
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_StructOneI64(&structOneI64_copy));
// CHECK-NEXT: StructOneI64.x = 42
  $s7Structs17printStructOneI64yyAA0cdE0VF(swift_interop_passDirect_Structs_StructOneI64(&structOneI64));
// CHECK-NEXT: StructOneI64.x = 42

  // printStructTwoI32(returnNewStructTwoI32(11))
  struct Structs_StructTwoI32 structTwoI32;
  swift_interop_returnDirect_Structs_StructTwoI32(&structTwoI32,
                                                  $s7Structs21returnNewStructTwoI32yAA0deF0Vs5Int32VF(11));
  $s7Structs17printStructTwoI32yyAA0cdE0VF(swift_interop_passDirect_Structs_StructTwoI32(&structTwoI32));
// CHECK-NEXT: StructTwoI32.x = 11, y = 22

  // printStructTwoI32(passThroughStructTwoI32(4, ..., 6))
  struct Structs_StructTwoI32 structTwoI32_copy;
  swift_interop_returnDirect_Structs_StructTwoI32(&structTwoI32_copy,
    $s7Structs23passThroughStructTwoI32yAA0deF0Vs5Int32V_AdFtF(4, swift_interop_passDirect_Structs_StructTwoI32(&structTwoI32), 6));
  $s7Structs17printStructTwoI32yyAA0cdE0VF(swift_interop_passDirect_Structs_StructTwoI32(&structTwoI32_copy));
// CHECK-NEXT: StructTwoI32.x = 15, y = 28

  // printStructStructTwoI32_and_OneI16AndOneStruct(... , returnNewStructOneI16AndOneStruct());
  struct Structs_StructOneI16AndOneStruct structOneI16AndOneStruct;
  swift_interop_returnDirect_Structs_StructOneI16AndOneStruct(&structOneI16AndOneStruct,
    $s7Structs024returnNewStructOneI16AndeD0AA0defgeD0VyF());
  $s7Structs011printStructc20TwoI32_and_OneI16AndgC0yyAA0cdE0V_AA0cghigC0VtF(
    swift_interop_passDirect_Structs_StructTwoI32(&structTwoI32),
    swift_interop_passDirect_Structs_StructOneI16AndOneStruct(&structOneI16AndOneStruct));
// CHECK-NEXT: StructTwoI32.x = 11, y = 22
// CHECK-NEXT: StructOneI16AndOneStruct.x = 255, y.x = 5, y.y = 72

  // let x = returnNewStructU16AndPointer(...)
  // getStructU16AndPointer_x(x)
  // getStructU16AndPointer_y(y)
  char c = 'A';
  struct Structs_StructU16AndPointer structU16AndPointer;
  swift_interop_returnDirect_Structs_StructU16AndPointer(&structU16AndPointer,
    $s7Structs28returnNewStructU16AndPointeryAA0defG0VSvF(&c));
  assert($s7Structs24getStructU16AndPointer_xys5UInt8VAA0cdeF0VF(
    swift_interop_passDirect_Structs_StructU16AndPointer(&structU16AndPointer)) == 55);
  assert($s7Structs24getStructU16AndPointer_yySvAA0cdeF0VF(
    swift_interop_passDirect_Structs_StructU16AndPointer(&structU16AndPointer)) == &c);

  // let x = returnNewStructDoubleAndFloat()
  // getStructDoubleAndFloat_x(x)
  // getStructDoubleAndFloat_y(x)
  double doubleValue = 1.25;
  float floatValue = -5.0f;
  struct Structs_StructDoubleAndFloat structDoubleAndFloat;
  swift_interop_returnDirect_Structs_StructDoubleAndFloat(&structDoubleAndFloat,
    $s7Structs29returnNewStructDoubleAndFloatyAA0defG0VSf_SdtF(floatValue, doubleValue));
  assert($s7Structs25getStructDoubleAndFloat_xySdAA0cdeF0VF(
    swift_interop_passDirect_Structs_StructDoubleAndFloat(&structDoubleAndFloat)) == doubleValue);
  assert($s7Structs25getStructDoubleAndFloat_yySfAA0cdeF0VF(
    swift_interop_passDirect_Structs_StructDoubleAndFloat(&structDoubleAndFloat)) == floatValue);

  // printStructI8AndU32AndI16(returnNewStructI8AndU32AndI16())
  struct Structs_StructI8AndU32AndI16 structI8AndU32AndI16;
  swift_interop_returnDirect_Structs_StructI8AndU32AndI16(&structI8AndU32AndI16,
    $s7Structs023returnNewStructI8AndU32F3I16AA0defgfH0VyF());
  $s7Structs019printStructI8AndU32E3I16yyAA0cdefeG0VF(
    swift_interop_passDirect_Structs_StructI8AndU32AndI16(&structI8AndU32AndI16));
// CHECK-NEXT: StructI8AndU32AndI16.x = -100, y = 123456, z = -3456
  return 0;
}
