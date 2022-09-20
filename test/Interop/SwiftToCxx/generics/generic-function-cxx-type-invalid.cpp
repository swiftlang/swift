// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/generic-function-in-cxx.swift -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=int
// RUN: not %target-interop-build-clangxx -std=gnu++20 -c %s -I %t -o /dev/null -DUSE_TYPE=CxxStruct

#include <cassert>
#include "functions.h"

struct CxxStruct { int x = 0; };

int main() {
  using namespace Functions;
    
  USE_TYPE value = {};
  genericPrintFunction(value);
  return 0;
}
