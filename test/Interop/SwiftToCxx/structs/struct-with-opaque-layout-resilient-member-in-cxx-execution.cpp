// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/resilient-struct-in-cxx.swift -enable-library-evolution -module-name Structs -emit-module -emit-module-path %t/Structs.swiftmodule

// RUN: %target-swift-frontend %S/struct-with-opaque-layout-resilient-member-in-cxx.swift -module-name UseStructs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/useStructs.h -I %t


// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o

// RUN: %target-interop-build-swift -c %S/resilient-struct-in-cxx.swift -enable-library-evolution -module-name Structs -o %t/resilient-struct-in-cxx.o -Xfrontend -entry-point-function-name -Xfrontend swiftMain2

// RUN: %target-interop-build-swift %S/struct-with-opaque-layout-resilient-member-in-cxx.swift -o %t/swift-structs-execution -Xlinker %t/resilient-struct-in-cxx.o -Xlinker %t/swift-structs-execution.o -module-name UseStructs -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution | %FileCheck --check-prefixes=CHECK,CURRENT %s

// REQUIRES: executable_test

#include <assert.h>
#include "useStructs.h"

int main() {
  using namespace UseStructs;
  auto s = createUsesResilientSmallStruct();
  s.dump();
// CHECK: UsesResilientSmallStruct(97,FirstSmallStruct(x: 65)
  return 0;
}
