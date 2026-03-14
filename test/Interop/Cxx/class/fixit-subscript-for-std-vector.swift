// For some reason, we are suggesting the "last" property instead of the subscript. (But only in this test, not locally.)
// XFAIL: *

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

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
import CxxStdlib.vector

public func test(v: V) {
  // CHECK: note: C++ method 'at' that returns a reference of type 'UnsafePointer' is unavailable.
  // CHECK: note: C++ method 'at' may return an interior pointer.
  // CHECK: note: Do you want to replace it with a call to the subscript operator?
  // CHECK: ~^~~ ~
  // CHECK: [ ]
  v.at(1)

  // CHECK: note: C++ method 'at' that returns a reference of type 'UnsafePointer' is unavailable.
  // CHECK: note: C++ method 'at' may return an interior pointer.
  // CHECK: note: Do you want to replace it with a call to the subscript operator?
  // CHECK: ~~^~~   ~
  // CHECK: [   ]
  v
    . at(42 )
}
