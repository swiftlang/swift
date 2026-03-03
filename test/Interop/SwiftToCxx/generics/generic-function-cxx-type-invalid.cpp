// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-function-in-cxx.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=int
// RUN: %target-interop-build-clangxx -std=gnu++17 -c %s -I %t -o /dev/null -DUSE_TYPE=int
// RUN: not %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=CxxStruct
// RUN: not %target-interop-build-clangxx -std=gnu++17 -c %s -I %t -o /dev/null -DUSE_TYPE=CxxStruct 2>&1 | %FileCheck -check-prefix=STATIC_ASSERT_ERROR %s

#include <cassert>
#include "functions.h"

struct CxxStruct { int x = 0; };

int main() {
  using namespace Functions;
    
  USE_TYPE value = {};
  genericPrintFunction(value);
  return 0;
}

// STATIC_ASSERT_ERROR: type cannot be used in a Swift generic context
