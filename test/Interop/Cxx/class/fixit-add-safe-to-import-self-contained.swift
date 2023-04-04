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
  // CHECK: note: annotate method 'test' with 'RETURNS_INDEPENDENT_VALUE' in C++ to make it available in Swift
  // CHECK: int *test() { }
  // CHECK: ^
  // CHECK: RETURNS_INDEPENDENT_VALUE
  
  x.test()
  
  // CHECK: note: annotate method 'other' with 'RETURNS_INDEPENDENT_VALUE' in C++ to make it available in Swift
  // CHECK: Ptr other() { }
  // CHECK: ^
  // CHECK: RETURNS_INDEPENDENT_VALUE
  
  // CHECK: note: Mark type 'Ptr' as 'SELF_CONTAINED' in C++ to make methods that use it available in Swift.
  // CHECK: struct Ptr {
  // CHECK: ^
  // CHECK: SELF_CONTAINED
  x.other()
}
