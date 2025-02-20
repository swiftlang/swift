// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-struct-in-cxx.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=int
// RUN: %target-interop-build-clangxx -std=gnu++17 -c %s -I %t -o /dev/null -DUSE_TYPE=int
// RUN: not %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=CxxStruct
// RUN: not %target-interop-build-clangxx -std=gnu++17 -c %s -I %t -o /dev/null -DUSE_TYPE=CxxStruct 2>&1 | %FileCheck -check-prefix=STATIC_ASSERT_ERROR %s

#include "structs.h"

struct CxxStruct { int x = 0; };

bool test(Structs::GenericPair<USE_TYPE, USE_TYPE> &val) {
  return sizeof(val) > 4;
}

// STATIC_ASSERT_ERROR: type cannot be used in a Swift generic context
