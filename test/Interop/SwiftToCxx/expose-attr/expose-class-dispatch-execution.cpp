// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/expose-class-dispatch.swift -module-name Lib -parse-as-library -typecheck -verify -emit-clang-header-path %t/Lib.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/main.o
// RUN: %target-interop-build-swift %S/expose-class-dispatch.swift -o %t/main -Xlinker %t/main.o -module-name Lib -parse-as-library -O -wmo -Xfrontend -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// With library evolution, the header calls the dispatch thunks of the methods.
// RUN: %empty-directory(%t-evo)
// RUN: %target-swift-frontend %S/expose-class-dispatch.swift -module-name Lib -parse-as-library -typecheck -verify -enable-library-evolution -emit-clang-header-path %t-evo/Lib.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-interop-build-clangxx -c %s -I %t-evo -o %t-evo/main.o
// RUN: %target-interop-build-swift %S/expose-class-dispatch.swift -o %t-evo/main -Xlinker %t-evo/main.o -module-name Lib -parse-as-library -O -wmo -enable-library-evolution -Xfrontend -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-codesign %t-evo/main
// RUN: %target-run %t-evo/main | %FileCheck %s

// REQUIRES: executable_test

#include "Lib.h"
#include <cstdio>

int main() {
  auto b = Lib::makeBase();
  auto d = Lib::makeDerived();
  auto h = Lib::makeHidden();
  auto p = Lib::makePrivate();
  std::printf("%lld %lld %lld %lld\n", (long long)b.value(), (long long)d.value(),
              (long long)h.value(), (long long)p.value());
  // CHECK: 1 3 4 5
  std::printf("%lld %lld %lld\n", (long long)b.getName(), (long long)d.getName(),
              (long long)h.getName());
  // CHECK-NEXT: 10 30 40
  return 0;
}
