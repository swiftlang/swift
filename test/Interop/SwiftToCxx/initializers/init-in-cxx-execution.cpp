// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/init-in-cxx.swift -module-name Init -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/inits.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-init-execution.o
// RUN: %target-interop-build-swift %S/init-in-cxx.swift -o %t/swift-init-execution -Xlinker %t/swift-init-execution.o -module-name Init -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-init-execution
// RUN: %target-run %t/swift-init-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "inits.h"

int main() {
  using namespace Init;

  auto smallStruct = FirstSmallStruct::init();
  assert(smallStruct.getX() == 42);
  auto smallStruct2 = FirstSmallStruct::init(24);
  assert(smallStruct2.getX() == 24);

  auto largeStruct = LargeStruct::init();
  assert(largeStruct.getX1() == 0);
  assert(largeStruct.getX2() == 0);
  assert(largeStruct.getX3() == 0);
  assert(largeStruct.getX4() == 0);
  assert(largeStruct.getX5() == 0);
  assert(largeStruct.getX6() == 0);
  auto largeStruct2 = LargeStruct::init(99, smallStruct2);
  assert(largeStruct2.getX1() == 99);
  assert(largeStruct2.getX2() == 24);
  assert(largeStruct2.getX3() == -99);
  assert(largeStruct2.getX4() == 0);
  assert(largeStruct2.getX5() == 11);
  assert(largeStruct2.getX6() == 99 * 24);

  {
    StructWithRefCountStoredProp structWithRefCountStoredProp =
      StructWithRefCountStoredProp::init();
    (void)structWithRefCountStoredProp;
    (void)StructWithRefCountStoredProp::init(22);
  }
// CHECK:      create RefCountedClass -1
// CHECK-NEXT: create RefCountedClass 22
// CHECK-NEXT: destroy RefCountedClass 22
// CHECK-NEXT: destroy RefCountedClass -1

  {
    auto x = FinalClass::init(FirstSmallStruct::init(78));
    assert(x.getProp().getX() == 78);
  }

  {
    auto x = DerivedClass::init(1, 2);
    assert(x.getX() == 5);
  }

  {
    auto x = DerivedClassTwo::init(1, 2);
    assert(x.getX() == 3);
  }
  return 0;
}
