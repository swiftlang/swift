// REQUIRES: std_span
// REQUIRES: swift_feature_Cells
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature Cells -enable-experimental-feature Lifetimes)

//--- Inputs/module.modulemap
module CellSpanReturn {
  header "cell-span-return.h"
  requires cplusplus
}

//--- Inputs/cell-span-return.h
#pragma once

#include <ptrcheck.h>
#include <span>

using IntSpan = std::span<int>;
using ConstIntSpan = std::span<const int>;

struct Container {
  int data[4] = {1, 2, 3, 4};
  IntSpan view() [[clang::lifetimebound]] { return IntSpan(data, 4); }
  ConstIntSpan cview() const [[clang::lifetimebound]] {
    return ConstIntSpan(data, 4);
  }
  IntSpan empty() [[clang::lifetimebound]] { return IntSpan(); }
  int sum() const { return data[0] + data[1] + data[2] + data[3]; }
};

inline int *__counted_by(n) countedView(Container &c [[clang::lifetimebound]],
                                        int n) {
  return c.data;
}

//--- test.swift
import StdlibUnittest
import CellSpanReturn

var suite = TestSuite("CellSpanReturn")

// A Span returned for an inout dependence keeps exclusive access to its source
// while it is alive, so each Span lives in its own scope.
suite.test("std::span") {
  var c = Container()
  do {
    let s: Span<Cell<CInt>> = c.view()
    expectEqual(s.count, 4)
    expectEqual(s[0].value, 1)
    s[0].value = 10
    s[3].value += 1
  }
  expectEqual(c.sum(), 10 + 2 + 3 + 5)

  let s: Span<ConstCell<CInt>> = c.cview()
  expectEqual(s.count, 4)
  expectEqual(s[0].value, 10)
}

suite.test("empty std::span") {
  var c = Container()
  let s: Span<Cell<CInt>> = c.empty()
  expectEqual(s.count, 0)
}

suite.test("counted_by") {
  var c = Container()
  do {
    let s: Span<Cell<CInt>> = countedView(&c, 4)
    expectEqual(s.count, 4)
    s[1].value = 20
  }
  expectEqual(c.sum(), 1 + 20 + 3 + 4)
}

runAllTests()
