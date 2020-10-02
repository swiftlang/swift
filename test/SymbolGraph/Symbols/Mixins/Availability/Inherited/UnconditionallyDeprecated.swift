// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DeprecatedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DeprecatedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DeprecatedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, deprecated)
public struct S {
  public func foo() {}
}

// CHECK-LABEL: "precise": "s:16DeprecatedFilled1SV3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "isUnconditionallyDeprecated": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]
