// RUN: %target-swift-frontend -parse-stdlib -target arm64-apple-macos11 -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -parse-stdlib -target arm64-apple-none -emit-ir %s -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -parse-stdlib -target arm64-apple-none-macho -emit-ir %s -enable-experimental-feature Embedded
// RUN: not %target-swift-frontend -parse-stdlib -target arm64-apple-macos11 -emit-ir %s -enable-objc-interop -enable-experimental-feature Embedded 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -target arm64-apple-macos11 -emit-ir %s -disable-objc-interop -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: error: Objective-C interoperability cannot be enabled with embedded Swift.
