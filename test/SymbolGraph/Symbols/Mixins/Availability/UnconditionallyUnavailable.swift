// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnconditionallyUnavailable -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnconditionallyUnavailable -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnconditionallyUnavailable.symbols.json

@available(*, unavailable)
public struct UnconditionallyUnavailable {}

// CHECK-NOT: domain
// CHECK: "isUnconditionallyUnavailable": true
