// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-class-in-cxx.swift -typecheck -module-name Class -clang-header-expose-public-decls -emit-clang-header-path %t/class.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o
// RUN: %target-interop-build-swift %S/swift-class-in-cxx.swift -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -module-name Class -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "class.h"
#include <cstdio>

extern "C" size_t swift_retainCount(void * _Nonnull obj);

size_t getRetainCount(const Class::ClassWithIntField & swiftClass) {
    void *p = Class::_impl::_impl_ClassWithIntField::getOpaquePointer(swiftClass);
    return swift_retainCount(p);
}

int main() {
  using namespace Class;

  // Ensure that the class is released.
  {
    auto x = returnClassWithIntField();
    assert(getRetainCount(x) == 1);
  }
// CHECK:      init ClassWithIntField
// CHECK-NEXT: destroy ClassWithIntField
    
  {
    auto x = returnClassWithIntField();
    {
      takeClassWithIntField(x);
      assert(getRetainCount(x) == 1);
      auto x2 = passThroughClassWithIntField(x);
      assert(getRetainCount(x) == 2);
      assert(getRetainCount(x2) == 2);
      takeClassWithIntField(x2);
      assert(getRetainCount(x) == 2);
    }
    assert(getRetainCount(x) == 1);
    takeClassWithIntField(x);
  }
// CHECK-NEXT: init ClassWithIntField
// CHECK-NEXT: ClassWithIntField: 0;
// CHECK-NEXT: ClassWithIntField: 42;
// CHECK-NEXT: ClassWithIntField: 42;
// CHECK-NEXT: destroy ClassWithIntField

  {
    auto x = returnClassWithIntField();
    assert(getRetainCount(x) == 1);
    takeClassWithIntFieldInout(x);
    assert(getRetainCount(x) == 1);
    takeClassWithIntField(x);
  }
// CHECK-NEXT: init ClassWithIntField
// CHECK-NEXT: init ClassWithIntField
// CHECK-NEXT: destroy ClassWithIntField
// CHECK-NEXT: ClassWithIntField: -11;
// CHECK-NEXT: destroy ClassWithIntField

  {
    auto x = returnClassWithIntField();
    {
      auto x2 = x;
      assert(getRetainCount(x) == 2);
    }
    assert(getRetainCount(x) == 1);
  }
// CHECK-NEXT: init ClassWithIntField
// CHECK-NEXT: destroy ClassWithIntField
  return 0;
}
