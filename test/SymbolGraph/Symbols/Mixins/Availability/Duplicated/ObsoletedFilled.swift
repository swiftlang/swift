// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ObsoletedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ObsoletedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ObsoletedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.0)
@available(macOS, obsoleted: 999.0)
public func foo() {}

// CHECK-LABEL: "precise": "s:15ObsoletedFilled3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "introduced": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 0
// CHECK-NEXT:     },
// CHECK-NEXT:     "obsoleted": {
// CHECK-NEXT:       "major": 999,
// CHECK-NEXT:       "minor": 0
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]
