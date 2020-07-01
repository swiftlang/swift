// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnconditionallyUnavailable -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnconditionallyUnavailable -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnconditionallyUnavailable.symbols.json

// REQUIRES: OS=macosx

@available(iOS, unavailable)
public struct UnconditionallyUnavailable {}

// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "iOS",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]
