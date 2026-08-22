// Ensure that `@cxx` functions are never emitted into a generated clang
// header.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -typecheck %s \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   -emit-clang-header-path %t/header.h
// RUN: %FileCheck %s < %t/header.h

// REQUIRES: swift_feature_CxxImplementation

import Functions

@cxx @implementation
public func foo(_ x: Int32) -> Int32 {
  return x
}

// CHECK: Unavailable in C++: Swift global function 'foo
