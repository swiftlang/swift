// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -typecheck -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -diagnostic-style llvm 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module Test {
    header "test.h"
    requires cplusplus
}

//--- Inputs/test.h
struct Ptr { int *p; };

struct X {
  X(const X&);
  
  int *test() { }
  Ptr other() { }
};

//--- test.swift

import Test

public func test(x: inout X) {
  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'test()'
  // CHECK: note: this returns a pointer or reference into a type that owns its storage
  x.test()

  // CHECK: error: expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'
  // CHECK: unsafe
  // CHECK: note: reference to unsafe instance method 'other()'
  // CHECK: note: this returns a view into a type that owns its storage
  x.other()
}
