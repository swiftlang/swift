// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-enum-extract-payload.swift -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/swift-enum-extract-payload.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

int main() {
  {
    auto xyz = makeXyz(1);
    assert(xyz.isFirst());
    assert(checkFoo(Foo::init(1234), xyz.getFirst()));
    assert(1234 == xyz.getFirst().getX());
  }
  {
    auto xyz = makeXyz(2);
    assert(xyz.isSecond());
    assert(checkUvw(makeUvw(2), xyz.getSecond()));
    assert(xyz.getSecond().isTwo());
  }
  {
    auto xyz = makeXyz(3);
    assert(xyz.isThird());
    assert(checkBar(Bar::init(1.1, 2.2, 3.3, 4.4, 5.5, 6.6), xyz.getThird()));
    assert(1.1 == xyz.getThird().getX1());
    assert(2.2 == xyz.getThird().getX2());
    assert(3.3 == xyz.getThird().getX3());
    assert(4.4 == xyz.getThird().getX4());
    assert(5.5 == xyz.getThird().getX5());
    assert(6.6 == xyz.getThird().getX6());
  }
  {
    auto e = makePrimitivePayload(1);
    assert(e.isX());
    assert(e.getX() == 9999);
  }
  {
    auto e = makePrimitivePayload(2);
    assert(e.isY());
    assert(e.getY() == 3.14);
  }
  {
    auto e = makePrimitivePayload(3);
    assert(e.isZ());
    assert(e.getZ());
  }
  return 0;
}
