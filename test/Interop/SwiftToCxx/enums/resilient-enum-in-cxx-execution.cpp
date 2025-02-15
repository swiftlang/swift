// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/resilient-enum-in-cxx.swift -enable-library-evolution -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o

// RUN: %target-interop-build-swift %S/resilient-enum-in-cxx.swift -enable-library-evolution -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution | %FileCheck --check-prefixes=CHECK,OLD_CASE %s

// RUN: %target-interop-build-swift %S/resilient-enum-in-cxx.swift -enable-library-evolution -o %t//swift-enums-execution-new -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain -D NEW_CASE
// RUN: %target-codesign %t/swift-enums-execution-new
// RUN: %target-run %t/swift-enums-execution-new | %FileCheck --check-prefixes=CHECK,NEW_CASE %s

// REQUIRES: executable_test

#include <cassert>
#include <iostream>
#include "enums.h"

using namespace Enums;

void useFooInSwitch(const Foo& f) {
  switch (f) {
    case Foo::a:
      std::cout << "Foo::a\n";
      break;;
    case Foo::unknownDefault:
      std::cout << "Foo::unknownDefault\n";
      break;
  }
}

int main() {
  auto f1 = makeFoo(10);
  auto f2 = makeFoo(-10);

  printFoo(f1);
  printFoo(f2);

  assert(!f2.isUnknownDefault());
  if (f1.isUnknownDefault()) {
    std::cout << "f1.inResilientUnknownCase()\n";
    assert(!f1.isA());
  } else {
    assert(f1.isA());
    assert(f1.getA() == 10.0);
  }

  useFooInSwitch(f1);
  useFooInSwitch(f2);

  return 0;
}

// OLD_CASE: a(10.0)
// NEW_CASE: b(10)
// CHECK-NEXT: a(-10.0)

// NEW_CASE: f1.inResilientUnknownCase()

// NEW_CASE: Foo::unknownDefault
// OLD_CASE: Foo::a
// CHECK-NEXT: Foo::a
