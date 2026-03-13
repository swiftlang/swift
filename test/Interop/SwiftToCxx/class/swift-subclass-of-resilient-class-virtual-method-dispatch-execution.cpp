// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-subclass-of-resilient-class-virtual-method-dispatch.swift -D RESILIENT_MODULE -module-name Class -emit-module -emit-module-path %t/Class.swiftmodule -enable-library-evolution -clang-header-expose-decls=all-public -emit-clang-header-path %t/class.h

// RUN: %target-swift-frontend %S/swift-subclass-of-resilient-class-virtual-method-dispatch.swift -I %t -module-name UseClass -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/useclass.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o

// RUN: %target-interop-build-swift -c %S/swift-subclass-of-resilient-class-virtual-method-dispatch.swift -D RESILIENT_MODULE -o %t/class.o -module-name Class -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain2

// RUN: %target-interop-build-swift %S/swift-subclass-of-resilient-class-virtual-method-dispatch.swift -I %t -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -Xlinker %t/class.o  -module-name UseClass -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution | %FileCheck %s

// REQUIRES: executable_test

#include "class.h"
#include "useclass.h"
#include <assert.h>

using namespace UseClass;

int main() {
  auto derived = createCrossModuleDerivedClass();
  Class::BaseClass derivedAsBase = derived;
  auto derivedDerived = createCrossModuleDerivedDerivedClass();
  CrossModuleDerivedClass derivedDerivedAsDerived = derivedDerived;

  {
    derived.virtualMethod();
    assert(derived.getComputedProp() == -56);
// CHECK: CrossModuleDerivedClass.virtualMethod
  }

  {
    derived.virtualMethodInDerived();
// CHECK-NEXT: CrossModuleDerivedClass.virtualMethodInDerived
    derivedDerived.virtualMethodInDerived();
// CHECK-NEXT: CrossModuleDerivedDerivedClass.virtualMethodInDerived
    derivedDerivedAsDerived.virtualMethodInDerived();
// CHECK-NEXT: CrossModuleDerivedDerivedClass.virtualMethodInDerived
  }

  {
    derived.virtualMethod2InDerived();
// CHECK-NEXT: CrossModuleDerivedClass.virtualMethod2InDerived
    derivedDerived.virtualMethod2InDerived();
// CHECK-NEXT: CrossModuleDerivedDerivedClass.virtualMethod2InDerived
    derivedDerivedAsDerived.virtualMethod2InDerived();
// CHECK-NEXT: CrossModuleDerivedDerivedClass.virtualMethod2InDerived
  }

  {
    swift::Int x;
    x = derived.getDerivedComputedProp();
    assert(x == 23);
    x = derivedDerived.getDerivedComputedProp();
    assert(x == -95);
    x = derivedDerivedAsDerived.getDerivedComputedProp();
    assert(x == -95);
  }
  return 0;
}
