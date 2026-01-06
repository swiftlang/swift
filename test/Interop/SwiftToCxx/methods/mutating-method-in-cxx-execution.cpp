// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/mutating-method-in-cxx.swift -module-name Methods -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/methods.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-methods-execution.o
// RUN: %target-interop-build-swift %S/mutating-method-in-cxx.swift -o %t/swift-methods-execution -Xlinker %t/swift-methods-execution.o -module-name Methods -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-methods-execution
// RUN: %target-run %t/swift-methods-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "methods.h"

int main() {
  using namespace Methods;

  auto smallStruct = createSmallStruct(10.0f);
  smallStruct.dump();
// CHECK: small x = 10.0;

  smallStruct.scale(0.25f).dump();
  smallStruct.dump();
// CHECK-NEXT: small x = 10.0;
// CHECK-NEXT: small x = 2.5;

  smallStruct.invert();
  smallStruct.dump();
// CHECK-NEXT: small x = -2.5;

  auto largeStruct = createLargeStruct();
  largeStruct.dump();
// CHECK-NEXT: 1, -5, 9, 11, 48879, -77

  largeStruct.double_();
  largeStruct.dump();
// CHECK-NEXT: 2, -10, 18, 22, 97758, -154

  largeStruct.scale(-3, 10).dump();
  largeStruct.dump();
// CHECK-NEXT: -6, -100, -54, 220, -293274, -1540
// CHECK-NEXT: -6, -100, -54, 220, -293274, -1540
  return 0;
}
