// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/struct-with-refcounted-member.swift -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o
// RUN: %target-interop-build-swift %S/struct-with-refcounted-member.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck %s

// REQUIRES: executable_test

#include <assert.h>
#include "structs.h"

int main() {
  using namespace Structs;

  // Ensure that the value destructor is called.
  {
    StructWithRefcountedMember value = returnNewStructWithRefcountedMember();
  }
// CHECK:      create RefCountedClass
// CHECK-NEXT: destroy RefCountedClass
  return 0;
}
