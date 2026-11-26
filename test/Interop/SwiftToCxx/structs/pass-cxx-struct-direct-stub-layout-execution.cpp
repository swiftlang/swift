// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/pass-cxx-struct-direct-stub-layout.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o
// RUN: %target-interop-build-swift %S/pass-cxx-struct-direct-stub-layout.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

#include "structs.h"

int main() {
  using namespace Structs;

  {
    auto x = returnNewStructBoolAndPair();
    printStructBoolAndPair(x);
    printStructBoolAndPair(passThroughStructBoolAndPair(x));
  }
// CHECK:      StructBoolAndPair(true, -2, 1234605616436508552)
// CHECK-NEXT: StructBoolAndPair(true, -2, 1234605616436508552)

  {
    auto x = returnNewStructFloatBoolAndPair();
    printStructFloatBoolAndPair(x);
    printStructFloatBoolAndPair(passThroughStructFloatBoolAndPair(x));
  }
// CHECK-NEXT: StructFloatBoolAndPair(1.5, true, -2, 2.5)
// CHECK-NEXT: StructFloatBoolAndPair(1.5, true, -2, 2.5)

  {
    auto x = returnNewOuter();
    printOuter(x);
    printOuter(passThroughOuter(x));
  }
// CHECK-NEXT: Outer(127, Inner(85, -3))
// CHECK-NEXT: Outer(127, Inner(85, -3))

  {
    auto x = returnNewOuterCascade();
    printOuterCascade(x);
    printOuterCascade(passThroughOuterCascade(x));
  }
// CHECK-NEXT: OuterCascade(1, 2, 3, -4)
// CHECK-NEXT: OuterCascade(1, 2, 3, -4)
  return 0;
}
