// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/small-structs-pass-return-direct-in-cxx.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o -Wno-incompatible-pointer-types
// RUN: %target-interop-build-swift %S/small-structs-pass-return-direct-in-cxx.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "structs.h"

int main() {
  using namespace Structs;

  static_assert(sizeof(StructOneI64) == 8, "");
  static_assert(sizeof(StructTwoI32) == 8, "");
  static_assert(sizeof(StructOneI16AndOneStruct) == 12, "");
  static_assert(sizeof(StructU16AndPointer) == (sizeof(void *) * 2), "");
  static_assert(sizeof(StructDoubleAndFloat) == 16, "");

  StructOneI64 structOneI64 = returnNewStructOneI64();
  printStructOneI64(structOneI64);
// CHECK:      StructOneI64.x = 42

  printStructOneI64(passThroughStructOneI64(structOneI64));
// CHECK-NEXT: StructOneI64.x = 42

  printStructTwoI32(passThroughStructTwoI32(1, returnNewStructTwoI32(5), 4));
// CHECK-NEXT: StructTwoI32.x = 6, y = 14

  printStructStructTwoI32_and_OneI16AndOneStruct(
    returnNewStructTwoI32(7), returnNewStructOneI16AndOneStruct());
// CHECK-NEXT: StructTwoI32.x = 7, y = 14
// CHECK-NEXT: StructOneI16AndOneStruct.x = 255, y.x = 5, y.y = 72

  char c = 'A';
  auto structU16AndPointer = returnNewStructU16AndPointer(&c);
  assert(getStructU16AndPointer_x(structU16AndPointer) == 55);
  assert(getStructU16AndPointer_y(structU16AndPointer) == &c);

  double doubleValue = 1.25;
  float floatValue = -5.0f;
  auto structDoubleAndFloat = returnNewStructDoubleAndFloat(floatValue, doubleValue);
  assert(getStructDoubleAndFloat_x(structDoubleAndFloat) == doubleValue);
  assert(getStructDoubleAndFloat_y(structDoubleAndFloat) == floatValue);

  // s = StructOneI16AndOneStruct(x: 0xFF, y: StructTwoI32(x: 5, y: 72))
  auto s = returnNewStructOneI16AndOneStruct();
  // s2 = StructTwoI32(x: 10, y: 20)
  auto s2 = returnNewStructTwoI32(10);
  inoutStructOneI16AndOneStruct(s, s2);
  printStructStructTwoI32_and_OneI16AndOneStruct(s2, s);
// CHECK-NEXT: StructTwoI32.x = 10, y = 20
// CHECK-NEXT: StructOneI16AndOneStruct.x = 205, y.x = 10, y.y = 20

  inoutStructDoubleAndFloat(structDoubleAndFloat);
  assert(getStructDoubleAndFloat_x(structDoubleAndFloat) == doubleValue * floatValue);
  assert(getStructDoubleAndFloat_y(structDoubleAndFloat) == floatValue / 10);
  return 0;
}
