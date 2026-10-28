// A `@cxx @implementation` function provides the body of a C++ function. Its
// only callers are in C++, which the Swift compiler cannot see. If the Swift
// function is not `public`, it has hidden linkage, and at -O the SIL
// optimizer's dead-function elimination would delete it as unused, unless we
// mark it as referenced from a foreign language.
//
// This test ensures that a non-public @cxx @implementation function is not
// removed by dead-function elimination.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   -O \
// RUN:   %s | %FileCheck %s --check-prefix=CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Functions

// CHECK-SYSV: define {{.*}}@_Z10returnsIntv
// CHECK-WIN: define {{.*}}@"?returnsInt@@YAHXZ"
@cxx @implementation
func returnsInt() -> CInt { return 42 }
