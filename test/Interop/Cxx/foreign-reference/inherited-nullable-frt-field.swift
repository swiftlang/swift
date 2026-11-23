// Reading a null foreign reference pointer field inherited from a C++ base
// class must not call the retain function with a null pointer.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Onone)
// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -O)

// REQUIRES: executable_test

//--- Inputs/module.modulemap
module M {
  header "m.h"
  requires cplusplus
}

//--- Inputs/m.h
#include <swift/bridging>

struct SWIFT_SHARED_REFERENCE(retainRC, releaseRC) RC {
  int refCount = 1;
  int value = 42;
};

// Deliberately no null checks: the importer must not call these with null.
inline void retainRC(RC *p) { p->refCount++; }
inline void releaseRC(RC *p) {
  if (--p->refCount == 0)
    delete p;
}

inline RC *makeRC() SWIFT_RETURNS_RETAINED { return new RC(); }

struct Base {
  RC *ref = nullptr;
};
struct Derived : Base {};

//--- test.swift
import M
import StdlibUnittest

var Suite = TestSuite("InheritedNullableFRTField")

Suite.test("null field of a base") {
  let d = Derived()
  expectNil(d.ref)
}

Suite.test("non-null field of a base") {
  let rc = makeRC()
  var d = Derived()
  d.ref = rc
  withExtendedLifetime(rc) {
    expectEqual(d.ref?.value, 42)
  }
}

runAllTests()
