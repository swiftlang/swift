// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h

void acceptRValueRef(int &&);

//--- test.swift

import Test

public func test() {
  var x: CInt = 2
  acceptRValueRef(x)
  // CHECK: note: function 'acceptRValueRef' unavailable (cannot import)
  // CHECK: note: C++ functions with rvalue reference parameters are unavailable in Swift
}
