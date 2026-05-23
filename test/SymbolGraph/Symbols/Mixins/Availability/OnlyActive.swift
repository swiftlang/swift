// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name OnlyActive -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name OnlyActive -I %t -pretty-print -active-platform-availability-only -output-dir %t
// RUN: %FileCheck %s --input-file %t/OnlyActive.symbols.json

// REQUIRES: OS=macosx

@available(macOS 10.9, iOS 8.0, watchOS 8.0, tvOS 8.0, visionOS 1.0, *)
public struct S {}

// CHECK-NOT: iOS
// CHECK: "domain": "macOS"
// CHECK-NOT: tvOS
// CHECK-NOT: visionOS
// CHECK-NOT: watchOS

