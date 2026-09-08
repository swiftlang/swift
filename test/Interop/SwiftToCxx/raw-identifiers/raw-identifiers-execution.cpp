// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/raw-identifiers-in-cxx.swift -module-name RawIdentifiers -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/raw.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/raw-identifiers-execution.o
// RUN: %target-interop-build-swift %S/raw-identifiers-in-cxx.swift -o %t/raw-identifiers-execution -Xlinker %t/raw-identifiers-execution.o -module-name RawIdentifiers -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/raw-identifiers-execution
// RUN: %target-run %t/raw-identifiers-execution

// REQUIRES: executable_test

#include <cassert>
#include "raw.h"

using namespace RawIdentifiers;

int switchTest(const Enum_u0020Name &e) {
  switch (e) {
  case Enum_u0020Name::default_:
    assert(e.isDefault_());
    return 0;
  case Enum_u0020Name::_1:
    assert(e.is_1());
    return 1;
  case Enum_u0020Name::_2:
    assert(e.is_2());
    assert(e.get_2() == 24);
    return 2;
  case Enum_u0020Name::hello_u0020world:
    assert(e.isHello_u0020world());
    return 3;
  }
}

int main() {
  assert(hello_u0020world() == 42);
  assert(_u00FCber(41) == 42);
  assert(_U0001F680speed() == 100);

  auto s = Struct_u0020Name::init(21);
  assert(s.getProp_u0020name() == 21);
  assert(s.method_u0020name() == 42);
  s.setProp_u0020name(50);
  assert(s.getProp_u0020name() == 50);
  assert(s.method_u0020name() == 100);

  {
    auto e = Enum_u0020Name::default_();
    assert(switchTest(e) == 0);
  }
  {
    auto e = Enum_u0020Name::_1();
    assert(switchTest(e) == 1);
  }
  {
    auto e = Enum_u0020Name::_2(24);
    assert(switchTest(e) == 2);
  }
  {
    auto e = Enum_u0020Name::hello_u0020world();
    assert(switchTest(e) == 3);
  }
  return 0;
}
