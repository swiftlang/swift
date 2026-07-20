// Ensure that `@cxx` functions are never emitted into a generated clang
// header: they implement declarations that already exist in the imported
// C++ header.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify %s \
// RUN:   -module-name Functions \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   -clang-header-expose-decls=all-public \
// RUN:   -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// The C++ header that declares `foo` and the generated header must be usable
// together in one translation unit.
// RUN: echo '#include "cxx-functions.h"' > %t/combined.h
// RUN: cat %t/functions.h >> %t/combined.h
// RUN: %check-interop-cxx-header-in-clang(-I %S/Inputs %t/combined.h)

// REQUIRES: swift_feature_CxxImplementation

import CxxFunctions

@cxx @implementation
public func foo(_ x: Int32) -> Int32 {
  return x
}

// CHECK-NOT: foo
// CHECK-LABEL: namespace Functions SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Functions") {
// CHECK-NOT: foo
// CHECK: // Unavailable in C++: Swift global function 'foo(_:)'.
// CHECK-NOT: foo
// CHECK: } // namespace Functions
// CHECK-NOT: foo
