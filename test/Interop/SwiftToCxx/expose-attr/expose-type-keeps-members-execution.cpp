// The generated header only prints exposed declarations.

// RUN: %empty-directory(%t-exposed)
// RUN: %target-swift-frontend %S/expose-type-keeps-members.swift -module-name Lib -parse-as-library -typecheck -verify -emit-clang-header-path %t-exposed/Lib.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-interop-build-clangxx -c %s -I %t-exposed -o %t-exposed/main.o
// RUN: %target-interop-build-swift %S/expose-type-keeps-members.swift -o %t-exposed/main -Xlinker %t-exposed/main.o -module-name Lib -parse-as-library -O -wmo -Xfrontend -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %target-codesign %t-exposed/main
// RUN: %target-run %t-exposed/main | %FileCheck %s

// With C++ interop, the generated header prints all internal declarations.

// RUN: %empty-directory(%t-all)
// RUN: %target-swift-frontend %S/expose-type-keeps-members.swift -module-name Lib -parse-as-library -typecheck -verify -emit-clang-header-path %t-all/Lib.h -cxx-interoperability-mode=default -emit-clang-header-min-access internal
// RUN: %target-interop-build-clangxx -c %s -I %t-all -o %t-all/main.o -DALL
// RUN: %target-interop-build-swift %S/expose-type-keeps-members.swift -o %t-all/main -Xlinker %t-all/main.o -module-name Lib -parse-as-library -O -wmo -cxx-interoperability-mode=default -emit-clang-header-min-access internal
// RUN: %target-codesign %t-all/main
// RUN: %target-run %t-all/main | %FileCheck %s --check-prefixes=CHECK,ALL

// REQUIRES: executable_test

#include "Lib.h"
#include <cstdio>

int main() {
  auto c = Lib::makeCounter();
  c.increment();
  c.increment();
  c.setObserved(5);
  c.setCached(8);
  std::printf("%lld %lld %lld %lld\n", (long long)c.value(),
              (long long)c.getCount(), (long long)c.getObserved(),
              (long long)c.getCached());
  // CHECK: 3 3 5 8

  auto p = Lib::Point::init(3, 4);
  auto q = p;
  q.setX(5);
  Lib::Point::setOrigin(6);
  std::printf("%lld %lld %lld %lld\n", (long long)q.sum(), (long long)p.getX(),
              (long long)p.extensionMethod(),
              (long long)Lib::Point::getOrigin());
  // CHECK-NEXT: 9 3 42 6

  auto w = Lib::Wrapper<swift::Int>::init(11);
  std::printf("%lld\n", (long long)w.get());
  // CHECK-NEXT: 11

  auto d = Lib::Direction::up();
  std::printf("%d %d\n", (int)(d.flipped() == Lib::Direction::down),
              (int)(d.getHashValue() == d.getHashValue()));
  // CHECK-NEXT: 1 1

#ifdef ALL
  auto h = Lib::NotExposed::init(7);
  auto r = Lib::Raw::two();
  std::printf("%lld %lld\n", (long long)h.hidden(), (long long)r.getRawValue());
  // ALL-NEXT: 7 2
#endif
  return 0;
}
