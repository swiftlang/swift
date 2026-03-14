// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/setter-in-cxx.swift -module-name Properties -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/properties.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-props-execution.o
// RUN: %target-interop-build-swift %S/setter-in-cxx.swift -o %t/swift-props-execution -Xlinker %t/swift-props-execution.o -module-name Properties -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-props-execution
// RUN: %target-run %t/swift-props-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "properties.h"

int main() {
  using namespace Properties;

  auto smallStructWithProps = createSmallStructWithProps();
  smallStructWithProps.setStoredInt(12);
  assert(smallStructWithProps.getStoredInt() == 12);
  assert(smallStructWithProps.getComputedInt() == 14);
  smallStructWithProps.setComputedInt(45);
  assert(smallStructWithProps.getStoredInt() == 43);
  assert(smallStructWithProps.getComputedInt() == 45);
    
  auto largeStructWithProps = smallStructWithProps.getLargeStructWithProps();
  assert(largeStructWithProps.getStoredSmallStruct().getX() == 0xFAE);
  largeStructWithProps.setStoredSmallStruct(createFirstSmallStruct(999));
  assert(largeStructWithProps.getStoredSmallStruct().getX() == 999);

  auto firstSmallStruct = largeStructWithProps.getStoredSmallStruct();
  assert(firstSmallStruct.getX() == 999);
  firstSmallStruct.setX(42);
  assert(firstSmallStruct.getX() == 42);

  largeStructWithProps.setStoredLargeStruct(largeStructWithProps.getStoredLargeStruct());

  smallStructWithProps.setLargeStructWithProps(largeStructWithProps);
// CHECK: SET: LargeStruct(x1: 90, x2: 1, x3: 2, x4: 3, x5: 4, x6: 5), FirstSmallStruct(x: 999)
    
  auto largeStruct = largeStructWithProps.getStoredLargeStruct();
  largeStruct.setX1(0);
  largeStruct.setX2(largeStruct.getX2() * 2);
  largeStruct.setX3(-72);
  largeStructWithProps.setStoredLargeStruct(largeStruct);

  smallStructWithProps.setLargeStructWithProps(largeStructWithProps);
// CHECK-NEXT: SET: LargeStruct(x1: 0, x2: 2, x3: -72, x4: 3, x5: 4, x6: 5), FirstSmallStruct(x: 999)

  auto propsInClass = createPropsInClass(-1234);
  assert(propsInClass.getStoredInt() == -1234);
  propsInClass.setStoredInt(45);
  assert(propsInClass.getStoredInt() == 45);
  propsInClass.setComputedInt(-11);
  assert(propsInClass.getComputedInt() == -11);
  assert(propsInClass.getStoredInt() == -13);

  {
    auto x = LargeStruct::getStaticX();
    assert(x == 0);
    LargeStruct::setStaticX(13);
    x = LargeStruct::getStaticX();
    assert(x == 13);
  }
  return 0;
}
