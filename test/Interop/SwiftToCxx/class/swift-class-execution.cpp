// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-class-in-cxx.swift -typecheck -module-name Class -clang-header-expose-public-decls -emit-clang-header-path %t/class.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-class-execution.o
// RUN: %target-interop-build-swift %S/swift-class-in-cxx.swift -o %t/swift-class-execution -Xlinker %t/swift-class-execution.o -module-name Class -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-class-execution
// RUN: %target-run %t/swift-class-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "class.h"

int main() {
  using namespace Class;

  // Ensure that the class is released.
  auto x = returnClassWithIntField();
// CHECK:      init ClassWithIntField
// CHECK-NEXT: destroy ClassWithIntField
  return 0;
}
