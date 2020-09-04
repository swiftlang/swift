// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnconditionallyDeprecated -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnconditionallyDeprecated -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnconditionallyDeprecated.symbols.json

@available(*, deprecated)
public struct UnconditionallyDeprecated {}

// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "*",
// CHECK-NEXT:     "isUnconditionallyDeprecated": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]
