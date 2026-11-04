// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Cells
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// The SIL verifier rejects the store_borrow scope around Ref<Cell<T>> under opaque values.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells \
// RUN:   -Xfrontend -disable-availability-checking)

//--- Inputs/module.modulemap
module RefReturn {
  header "ref-return.h"
  requires cplusplus
}

//--- Inputs/ref-return.h
#pragma once

struct Counter {
  int value = 0;
  int &get() [[clang::lifetimebound]] { return value; }
  const int &cget() const [[clang::lifetimebound]] { return value; }
  void increment() { value += 1; }
};

inline int &pick(int &a [[clang::lifetimebound]], int &b) { return a; }
inline const int &constPick(const int &a [[clang::lifetimebound]]) { return a; }

//--- test.swift
import StdlibUnittest
import RefReturn

var suite = TestSuite("ReferenceReturn")

// A Ref returned for an inout dependence keeps exclusive access to its source
// while it is alive, so each Ref lives in its own scope.
suite.test("free function") {
  var a: CInt = 1
  var b: CInt = 2
  do {
    let r: Ref<Cell<CInt>> = pick(&a, &b)
    expectEqual(r.value.value, 1)
    r.value.value = 42
  }
  expectEqual(a, 42)
  expectEqual(b, 2)

  let c: Ref<ConstCell<CInt>> = constPick(a)
  expectEqual(c.value.value, 42)
}

suite.test("method") {
  var counter = Counter()
  do {
    let r: Ref<Cell<CInt>> = counter.get()
    r.value.value = 5
    r.value.value += 1
  }
  expectEqual(counter.value, 6)

  let c: Ref<ConstCell<CInt>> = counter.cget()
  expectEqual(c.value.value, 6)
}

runAllTests()
