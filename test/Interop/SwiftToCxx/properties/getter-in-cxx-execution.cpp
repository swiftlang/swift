// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/getter-in-cxx.swift -module-name Properties -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/properties.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-props-execution.o
// RUN: %target-interop-build-swift %S/getter-in-cxx.swift -o %t/swift-props-execution -Xlinker %t/swift-props-execution.o -module-name Properties -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-props-execution
// RUN: %target-run %t/swift-props-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "properties.h"

int main() {
  using namespace Properties;

  auto smallStructWithGetter = createSmallStructWithGetter();
  assert(smallStructWithGetter.getStoredInt() == 21);
  assert(smallStructWithGetter.getComputedInt() == 23);

  auto largeStruct = smallStructWithGetter.getLargeStruct();
  assert(largeStruct.getX1() == 46);
  assert(largeStruct.getX2() == 1);
  assert(largeStruct.getX3() == 2);
  assert(largeStruct.getX4() == 3);
  assert(largeStruct.getX5() == 4);
  assert(largeStruct.getX6() == 5);

  auto largeStruct2 = largeStruct.getAnotherLargeStruct();
  assert(largeStruct2.getX1() == 11);
  assert(largeStruct2.getX2() == 42);
  assert(largeStruct2.getX3() == -0xFFF);
  assert(largeStruct2.getX4() == 0xbad);
  assert(largeStruct2.getX5() == 5);
  assert(largeStruct2.getX6() == 0);

  auto firstSmallStruct = largeStruct.getFirstSmallStruct();
  assert(firstSmallStruct.getX() == 65);

  auto smallStruct2 = smallStructWithGetter.getSmallStruct();
  assert(smallStruct2.getStoredInt() == 0xFAE);
  assert(smallStruct2.getComputedInt() == (0xFAE + 2));

  {
    StructWithRefCountStoredProp structWithRefCountStoredProp =
      createStructWithRefCountStoredProp();
    auto anotherOne = structWithRefCountStoredProp.getAnother();
    (void)anotherOne;
  }
// CHECK:      create RefCountedClass 0
// CHECK-NEXT: create RefCountedClass 1
// CHECK-NEXT: destroy RefCountedClass 1
// CHECK-NEXT: destroy RefCountedClass 0

  auto propsInClass = createPropsInClass(-1234);
  assert(propsInClass.getStoredInt() == -1234);
  assert(propsInClass.getComputedInt() == -1235);
  auto smallStructFromClass = propsInClass.getSmallStruct();
  assert(smallStructFromClass.getX() == 1234);

  {
    auto x = LargeStruct::getStaticX();
    assert(x == -402);
    auto smallStruct = LargeStruct::getStaticSmallStruct();
    assert(smallStruct.getX() == 789);
  }
  return 0;
}
