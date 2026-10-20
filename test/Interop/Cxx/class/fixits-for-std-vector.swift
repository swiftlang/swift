// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -diagnostic-style llvm 2>&1 | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
#include <vector>

using V = std::vector<int>;

//--- test.swift

import Test
import CxxStdlib

public func test(v: V) {
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'begin()'
  _ = v.begin()

  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'end()'
  _ = v.end()

  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'front()'
  _ = v.front()

  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'back()'
  _ = v.back()
}
