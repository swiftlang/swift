// RUN: not %target-swift-frontend -emit-ir %s -enable-library-evolution -enable-experimental-feature Embedded -wmo 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -emit-ir %s -enable-resilience -enable-experimental-feature Embedded -wmo 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: error: Library evolution cannot be enabled with embedded Swift.
