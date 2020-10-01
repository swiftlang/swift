// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IntroducedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IntroducedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IntroducedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, deprecated: 10.0)
@available(macOS, introduced: 10.0)
public func foo() {}

// This effectively erases the deprecation.

// CHECK: "precise": "s:16IntroducedFilled3fooyyF"
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "introduced": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 0
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]
