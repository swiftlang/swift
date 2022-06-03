// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-functions-errors.swift -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-errors-execution.o
// RUN: %target-interop-build-swift %S/swift-functions-errors.swift -o %t/swift-functions-errors-execution -Xlinker %t/swift-functions-errors-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-errors-execution
// RUN: %target-run %t/swift-functions-errors-execution | %FileCheck %s

// REQUIRES: executable_test
// XFAIL: *

#include <cassert>
#include "functions.h"

int main() {
  static_assert(!noexcept(Functions::emptyThrowFunction()), "noexcept function");
  static_assert(!noexcept(Functions::throwFunction()), "noexcept function");

  Functions::emptyThrowFunction();
  Functions::throwFunction();
  return 0;
}

// CHECK: passEmptyThrowFunction
// CHECK-NEXT: passThrowFunction