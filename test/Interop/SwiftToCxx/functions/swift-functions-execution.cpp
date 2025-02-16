// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-functions.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h  -cxx-interoperability-mode=upcoming-swift

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-execution.o -g
// RUN: %target-interop-build-swift %S/swift-functions.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain -g

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution 2>&1 | %FileCheck %s

// REQUIRES: executable_test

#include <cassert>
#include <stdio.h>
#include "functions.h"

int main() {
  static_assert(noexcept(Functions::passVoidReturnVoid()), "noexcept function");
  static_assert(noexcept(Functions::_impl::$s9Functions014passVoidReturnC0yyF()),
                "noexcept function");

  Functions::passVoidReturnVoid();
  Functions::passIntReturnVoid(-1);
  assert(Functions::passTwoIntReturnIntNoArgLabel(1, 2) == 42);
  assert(Functions::passTwoIntReturnInt(1, 2) == 3);
  assert(Functions::passTwoIntReturnIntNoArgLabelParamName(1, 4) == 5);
  Functions::passVoidReturnNever();
  return 42;
}

// CHECK: passVoidReturnVoid
// CHECK-NEXT: passIntReturnVoid -1
// CHECK-NEXT: passTwoIntReturnIntNoArgLabel
// CHECK-NEXT: passVoidReturnNever
