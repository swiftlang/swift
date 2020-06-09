// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DeprecatedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DeprecatedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DeprecatedFilled.symbols.json
// RUN: %FileCheck %s --input-file %t/DeprecatedFilled.symbols.json --check-prefix=TRANSITIVE

// REQUIRES: OS=macosx

@available(macOS, deprecated: 10.15)
public struct S {
  public func foo() {}
}

// CHECK-LABEL: "precise": "s:16DeprecatedFilled1SV3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "deprecated": {
// CHECK-NEXT:       "major": 10,
// CHECK-NEXT:       "minor": 15
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]

@available(macOS, deprecated: 10.15)
public struct Outer {
  public struct Inner {
    // TRANSITIVE-LABEL: "precise": "s:16DeprecatedFilled5OuterV5InnerV3fooyyF"
    // TRANSITIVE: "availability": [
    // TRANSITIVE-NEXT:   {
    // TRANSITIVE-NEXT:     "domain": "macOS",
    // TRANSITIVE-NEXT:     "deprecated": {
    // TRANSITIVE-NEXT:       "major": 10,
    // TRANSITIVE-NEXT:       "minor": 15
    // TRANSITIVE-NEXT:     }
    // TRANSITIVE-NEXT:   }
    // TRANSITIVE-NEXT: ]
    public func foo() {}
  }
}

