// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-function-in-cxx.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

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

  {
    int x = 4;
    assert(genericRet(x) == 4);
  }

  {
    double x = -19.75;
    assert(genericRet(x) == -19.75);
  }

  {
    auto tc = createTestClass();
    genericPrintFunction(tc);
  }
// CHECK-NEXT: TestClass value=Functions.TestClass
// CHECK-NEXT: deinit TestClass

  {
    auto tc = createTestClass();
    auto tc2 = genericRet(tc);
    assert(swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc) ==
           swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc2));
    genericPrintFunction(tc2);
  }
// CHECK-NEXT: TestClass value=Functions.TestClass
// CHECK-NEXT: deinit TestClass

  {
    auto tc = createTestClass();
    auto tc2 = createTestClass();
    const auto p1 = swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc);
    const auto p2 = swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc2);
    assert(p1 != p2);
    genericSwap(tc, tc2);
    assert(p2 == swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc));
    assert(p1 == swift::_impl::_impl_RefCountedClass::getOpaquePointer(tc2));
  }
// CHECK-NEXT: deinit TestClass
// CHECK-NEXT: deinit TestClass

  {
    auto x = createTestLargeStruct(0);
    genericPrintFunction(x);
  }
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: 0, x2: 1, x3: -1, x4: 0, x5: 2, x6: -2)

  {
    auto x = createTestLargeStruct(11);
    auto y = createTestLargeStruct(-9);
    genericPrintFunction(x);
    genericPrintFunction(y);
    genericSwap(x, y);
    genericPrintFunction(x);
    genericPrintFunction(y);
    auto xy = genericRet(x);
    genericPrintFunction(xy);
    xy.mut();
    genericPrintFunction(xy);
    genericPrintFunction(x);
  }
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: 11, x2: 12, x3: 10, x4: 11, x5: 13, x6: 9)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: -9, x2: -8, x3: -10, x4: -9, x5: -7, x6: -11)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: -9, x2: -8, x3: -10, x4: -9, x5: -7, x6: -11)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: 11, x2: 12, x3: 10, x4: 11, x5: 13, x6: 9)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: -9, x2: -8, x3: -10, x4: -9, x5: -7, x6: -11)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: 9, x2: -8, x3: -10, x4: -9, x5: -7, x6: -7)
// CHECK-NEXT: TestLargeStruct value=TestLargeStruct(x1: -9, x2: -8, x3: -10, x4: -9, x5: -7, x6: -11)

  {
    auto x = createTestSmallStruct(45);
    auto y = createTestSmallStruct(0xFed1);
    genericPrintFunction(x);
    genericPrintFunction(y);
    genericSwap(y, x);
    genericPrintFunction(x);
    genericPrintFunction(y);
    auto xy = genericRet(x);
    genericPrintFunction(xy);
    xy.mut();
    genericPrintFunction(xy);
    genericPrintFunction(x);
  }
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 45)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 65233)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 65233)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 45)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 65233)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 4294902062)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 65233)
  {
    auto x = createTestSmallStruct(42);
    int passThru = x.genericMethodPassThrough((int)555);
    assert(passThru == 555);
    auto xprime = x.genericMethodPassThrough(x);
    genericPrintFunction(xprime);
    x.genericMethodMutTake((uint32_t)16);
    genericPrintFunction(xprime);
    genericPrintFunction(x);
    x.genericMethodMutTake(xprime);
    genericPrintFunction(xprime);
    genericPrintFunction(x);
  }
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 42)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 42)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 58)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 42)
// CHECK-NEXT: TestSmallStruct value=TestSmallStruct(x1: 57)
  return 0;
}
