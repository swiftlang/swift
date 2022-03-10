// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/cdecl.swift -typecheck -module-name CdeclFunctions -emit-cxx-header-path %t/functions.h

// RUN: %target-interop-clangxx -c %s -I %t -o %t/cdecl-execution.o
// RUN: %target-build-swift-link-cxx %S/cdecl.swift -o %t/cdecl-execution -Xlinker %t/cdecl-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/cdecl-execution
// RUN: %target-run %t/cdecl-execution

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

int main() {
  assert(CdeclFunctions::differentCDeclName(1, 1) == 2);
  return 0;
}
