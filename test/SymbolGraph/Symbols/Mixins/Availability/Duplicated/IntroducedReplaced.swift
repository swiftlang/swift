// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IntroducedReplaced -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IntroducedReplaced -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IntroducedReplaced.symbols.json

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.12)
@available(macOS, introduced: 10.11)
@available(macOS, introduced: 10.10)
public func foo() {}

// Highest `introduced` wins.
// CHECK-LABEL: "precise": "s:18IntroducedReplaced3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "introduced": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 12
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]
