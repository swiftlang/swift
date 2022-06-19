// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/method-in-cxx.swift -typecheck -module-name Methods -clang-header-expose-public-decls -emit-clang-header-path %t/methods.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-methods-execution.o
// RUN: %target-interop-build-swift %S/method-in-cxx.swift -o %t/swift-methods-execution -Xlinker %t/swift-methods-execution.o -module-name Methods -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-methods-execution
// RUN: %target-run %t/swift-methods-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "methods.h"

int main() {
  using namespace Methods;

  auto largeStruct = createLargeStruct();
  largeStruct.dump();
// CHECK: -1, 2, -100, 42, 67, -10101

  LargeStruct largeStructDoubled = largeStruct.doubled();
  largeStructDoubled.dump();
// CHECK-NEXT: -2, 4, -200, 84, 134, -20202

  auto largeStructScaled = largeStruct.scaled(2, -4);
  largeStructScaled.dump();
// CHECK-NEXT: -2, -8, -200, -168, 134, 40404

  auto largeStructAdded = largeStructDoubled.added(largeStruct);
  largeStructAdded.dump();
// CHECK-NEXT: -3, 6, -300, 126, 201, -30303
  return 0;
}
