// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-class-virtual-method-dispatch.swift -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o
// RUN: %target-interop-build-swift %S/swift-class-virtual-method-dispatch.swift -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -module-name Class -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution | %FileCheck %s

// REQUIRES: executable_test

#include "class.h"
#include <assert.h>
#include <cstdio>

using namespace Class;

int main() {
  auto base = returnBaseClass();
  auto derived = returnDerivedClass();
  BaseClass derivedAsBase = derived;
  auto derivedDerived = returnDerivedDerivedClass();
  BaseClass derivedDerivedAsBase = derivedDerived;
  DerivedClass derivedDerivedAsDerived = derivedDerived;

  {
    base.virtualMethod();
// CHECK: BaseClass.virtualMethod
    puts("after base invoke");
    derived.virtualMethod();
    derivedAsBase.virtualMethod();
// CHECK-NEXT: after base invoke
// CHECK-NEXT: DerivedClass.virtualMethod
// CHECK-NEXT: DerivedClass.virtualMethod
    puts("after derived invoke");
    derivedDerived.virtualMethod();
    derivedDerivedAsBase.virtualMethod();
// CHECK-NEXT: after derived invoke
// CHECK-NEXT: DerivedDerivedClass.virtualMethod
// CHECK-NEXT: DerivedDerivedClass.virtualMethod
  }

  {
    swift::Int x;
    x = base.virtualMethodIntInt(5);
    assert(x == 5);

    x = derived.virtualMethodIntInt(5);
    assert(x == -5);
    x = derivedAsBase.virtualMethodIntInt(-13);
    assert(x == 13);

    x = derivedDerived.virtualMethodIntInt(789);
    assert(x == -789);
    x = derivedDerivedAsBase.virtualMethodIntInt(76);
    assert(x == -76);
  }

  {
    swift::Int x;
    x = base.finalMethodInBase(5);
    assert(x == 10);

    x = derived.finalMethodInBase(10);
    assert(x == 20);
    x = derivedAsBase.finalMethodInBase(30);
    assert(x == 60);

    x = derivedDerived.finalMethodInBase(11);
    assert(x == 22);
    x = derivedDerivedAsBase.finalMethodInBase(-22);
    assert(x == -44);
  }

  {
    auto obj = derived.virtualMethodInDerived(base);
    obj.virtualMethod();
// CHECK-NEXT: BaseClass.virtualMethod
    obj = derivedDerived.virtualMethodInDerived(base);
    obj.virtualMethod();
// CHECK-NEXT: DerivedDerivedClass.virtualMethod
    obj = derivedDerivedAsDerived.virtualMethodInDerived(base);
    obj.virtualMethod();
// CHECK-NEXT: DerivedDerivedClass.virtualMethod
  }

  {
    derivedDerived.methodInDerivedDerived();
// CHECK-NEXT: DerivedDerivedClass.methodInDerivedDerived
  }

  {
    swift::Int x;
    x = base.getVirtualComputedProp();
    assert(x == 21);

    x = derived.getVirtualComputedProp();
    assert(x == -75);
    x = derivedAsBase.getVirtualComputedProp();
    assert(x == -75);

    x = derivedDerived.getVirtualComputedProp();
    assert(x == -75);
    x = derivedDerivedAsBase.getVirtualComputedProp();
    assert(x == -75);
  }

  {
    int64_t x;
    x = base.getVirtualComputedGetSet();
    assert(x == 0);
    base.setVirtualComputedGetSet(45);
    x = base.getVirtualComputedGetSet();
    assert(x == 45);

    x = derived.getVirtualComputedGetSet();
    assert(x == 0);
    derivedAsBase.setVirtualComputedGetSet(9);
    x = derivedAsBase.getVirtualComputedGetSet();
    assert(x == 18);
    derived.setVirtualComputedGetSet(-1);
    x = derived.getVirtualComputedGetSet();
    assert(x == -2);

    x = derivedDerived.getVirtualComputedGetSet();
    assert(x == 0);
    derivedDerivedAsBase.setVirtualComputedGetSet(3);
    x = derivedDerivedAsBase.getVirtualComputedGetSet();
    assert(x == 6);
  }

  {
    swift::Int x;
    x = base.getStoredProp();
    assert(x == 0);

    derivedAsBase.setStoredProp(39);
    x = derivedAsBase.getStoredProp();
    assert(x == -39);

    x = derivedDerivedAsBase.getStoredProp();
    assert(x == 0);
    derivedDerivedAsBase.setStoredProp(843);
    x = derivedDerivedAsBase.getStoredProp();
    assert(x == 844);

    x = derivedDerived.getComputedPropInDerivedDerived();
    assert(x == 11);
  }

  {
    swift::Int x;
    x = base[23];
    assert(x == 23);

    x = derivedAsBase[23];
    assert(x == 46);
    x = derivedDerivedAsBase[-11];
    assert(x == -22);
  }
  return 0;
}
