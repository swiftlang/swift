// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: not %target-swift-frontend -emit-ir -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop 2>&1 | %FileCheck %s
//
// XFAIL: OS=linux-android, OS=linux-androideabi

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
#include <stdlib.h>

inline void* operator new(size_t, void* p) { return p; }

struct __attribute__((swift_attr("import_as_ref"))) Empty {
  static Empty *create() { return new (malloc(sizeof(Empty))) Empty(); }
};

//--- test.swift

import Test

public func test<T>(_ _: T) {}

// CHECK: error: attempt to use a foreign reference type in a generic context. Foreign reference types are currently not supported. Using foreign reference types in a generic context is not yet implemented.
test(Empty.create())
