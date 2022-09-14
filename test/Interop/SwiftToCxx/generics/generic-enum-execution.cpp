// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-enum-in-cxx.swift -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-enum-in-cxx.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// REQUIRES: executable_test

#include "generics.h"
#include <cassert>

int main() {
  using namespace Generics;

  {
    auto x = makeGenericOpt<int>(42);
    takeGenericOpt(x);
    // CHECK: some(42)
    inoutGenericOpt(x, -11);
    takeGenericOpt(x);
    // CHECK-NEXT: none
    inoutGenericOpt(x, -11);
    takeGenericOpt(x);
    // CHECK-NEXT: some(-11)
    x.method();
    // CHECK-NEXT: GenericOpt<T>::testme::some(-11);
    assert(x.getComputedProp() == 42);
    x.reset();
    assert(x.genericMethod<double>(5.25) == 5.25);
    // CHECK-NEXT: GenericOpt<T>::genericMethod<T>::none,5.25;
  }
  {
    auto x = makeConcreteOpt(0xFA);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(250) ;
    inoutConcreteOpt(x);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(750) ;
    inoutConcreteOpt(x);
    takeConcreteOpt(x);
    // CHECK-NEXT: CONCRETE opt: some(2250) ;
    x.method();
    // CHECK-NEXT: GenericOpt<T>::testme::some(2250);
    assert(x.getComputedProp() == 42);
    x.reset();
    assert(x.genericMethod<double>(-1.25) == -1.25);
    // CHECK-NEXT: GenericOpt<T>::genericMethod<T>::none,-1.25;
  }
  return 0;
}
