// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/large-structs-pass-return-indirect-in-cxx.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// Link should fail by default when a move is
// performed in C++.
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o
// RUN: %target-interop-build-swift %S/large-structs-pass-return-indirect-in-cxx.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain 2>&1

// Compile should fail by default when move assignment is attempted in C++:

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o -DMOVE_ASSIGN 2>&1

// Fallback to abort at runtime:

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-structs-execution.o
// RUN: %target-interop-build-swift %S/large-structs-pass-return-indirect-in-cxx.swift -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain 2>&1

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution 2>%t/output || true

// REQUIRES: executable_test

#include <assert.h>
#include "structs.h"
#include <memory>

int main() {
  using namespace Structs;

  auto x = returnNewStructSeveralI64(42);
#ifdef MOVE_ASSIGN
  auto y = returnNewStructSeveralI64(24);
  x = std::move(y);
#else
  StructSeveralI64 x2 = std::move(x);
#endif
  return 0;
}
