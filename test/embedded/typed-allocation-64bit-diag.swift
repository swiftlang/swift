// RUN: not %target-swift-frontend -parse-stdlib -target armv7-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -target arm64-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation

// CHECK: error: the 'TypedAllocation' feature is only supported on 64-bit targets
