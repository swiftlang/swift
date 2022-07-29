// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-function-in-cxx.swift -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/generic-function-in-cxx.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

int main() {
  using namespace Functions;

  static_assert(noexcept(genericPrintFunctionTwoArg<int>(0, 0)), "noexcept function");

  {
    int i = 2;
    genericPrintFunctionTwoArg(i, 3);
// CHECK:      X: 2
// CHECK-NEXT: Y: 3
  }

  {
    int8_t x = -1;
    genericPrintFunction(x);
// CHECK-NEXT: Int8 value=-1
  }

  {
    uint8_t x = 0xff;
    genericPrintFunction(x);
// CHECK-NEXT: UInt8 value=255
  }

  {
    int16_t x = -9856;
    genericPrintFunction(x);
// CHECK-NEXT: Int16 value=-9856
  }

  {
    uint16_t x = 3248;
    genericPrintFunction(x);
// CHECK-NEXT: UInt16 value=3248
  }

  {
    int32_t x = -0x12343;
    genericPrintFunction(x);
// CHECK-NEXT: Int32 value=-74563
  }

  {
    uint32_t x = 0xC0011334;
    genericPrintFunction(x);
// CHECK-NEXT: UInt32 value=3221295924
  }

  {
    int64_t x = -123456789123;
    genericPrintFunction(x);
// CHECK-NEXT: Int64 value=-123456789123
  }

  {
    uint64_t x = 0x12345678abcd1234;
    genericPrintFunction(x);
// CHECK-NEXT: UInt64 value=1311768467750064692
  }

  {
    float x = -54.0f;
    genericPrintFunction(x);
// CHECK-NEXT: Float value=-54.0
  }

  {
    double x = 1.25;
    genericPrintFunction(x);
// CHECK-NEXT: Double value=1.25
  }

  {
    bool x = true;
    genericPrintFunction(x);
// CHECK-NEXT: Bool value=true
  }

  {
    int i = 0;
    void *x = &i;
    genericPrintFunction(x);
// CHECK-NEXT: OpaquePointer value
  }

  {
    int i = 55;
    int j = 66;
    bool x = false;
    genericPrintFunctionMultiGeneric<int, bool>(-78, i, j, 45, x);
// CHECK-NEXT: Int32 value 1=55
// CHECK-NEXT: Int32 value 2=66
// CHECK-NEXT: Bool value 1=false
// CHECK-NEXT: other values=-78,45
  }

  {
    int x = 42;
    int y = -13;
    genericSwap(x, y);
    assert(x == -13);
    assert(y == 42);
  }
  return 0;
}
