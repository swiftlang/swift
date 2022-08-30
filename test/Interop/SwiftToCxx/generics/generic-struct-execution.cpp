// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o %t/swift-generics-execution.o
// RUN: %target-interop-build-swift %S/generic-struct-in-cxx.swift -o %t/swift-generics-execution -Xlinker %t/swift-generics-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-generics-execution
// RUN: %target-run %t/swift-generics-execution | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include "generics.h"

int main() {
  using namespace Generics;

  {
    auto x = makeGenericPair<int, int>(11, 42);
    takeGenericPair(x);
    // CHECK: GenericPair<Int32, Int32>(x: 11, y: 42)
    auto xprime = passThroughGenericPair(x, -995);
    takeGenericPair(x);
    takeGenericPair(xprime);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: 42)
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 11, y: -995)
    inoutGenericPair(x, 0xFF);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<Int32, Int32>(x: 255, y: 42)
  }

  {
    auto x = makeConcretePair(100000, 0x1fee7);
    takeGenericPair(x);
    // CHECK-NEXT: GenericPair<UInt32, UInt32>(x: 100000, y: 130791)
  }
  return 0;
}
