// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DeprecatedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DeprecatedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DeprecatedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.0)
@available(macOS, deprecated: 10.1)
public func foo() {}

// CHECK-LABEL: "precise": "s:16DeprecatedFilled3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "introduced": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 0
// CHECK-NEXT:     },
// CHECK-NEXT:     "deprecated": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 1
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]
