// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct Ptr { int *p; };

struct X {
  int *test() { }
  Ptr other() { }
};

//--- test.swift

import Test

public func test(x: X) {
  // CHECK: note: Mark method 'test' as 'safe_to_import' in C++ to make it available in Swift.
  // CHECK: int *test() { }
  // CHECK: ^
  // CHECK: __attribute__((swift_attr("safe_to_import")))
  
  x.test()
  
  // CHECK: note: Mark method 'other' as 'safe_to_import' in C++ to make it available in Swift.
  // CHECK: Ptr other() { }
  // CHECK: ^
  // CHECK: __attribute__((swift_attr("safe_to_import")))
  
  // CHECK: note: Mark type 'Ptr' as 'self_contained' in C++ to make methods that use it available in Swift.
  // CHECK: struct X {
  // CHECK: ^
  // CHECK: __attribute__((swift_attr("self_contained")))
  x.other()
}
