// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IntroducedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IntroducedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IntroducedFilled.symbols.json
// RUN: %FileCheck %s --input-file %t/IntroducedFilled.symbols.json --check-prefix=TRANSITIVE

// REQUIRES: OS=macosx

@available(macOS, obsoleted: 50.1)
public struct S {
  public func foo() {}
}

// CHECK-LABEL: "precise": "s:16IntroducedFilled1SV3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "obsoleted": {
// CHECK-NEXT:       "major": 50,
// CHECK-NEXT:       "minor": 1
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]

@available(macOS, obsoleted: 50.1)
public struct Outer {
  public struct Inner {
    // TRANSITIVE-LABEL: "precise": "s:16IntroducedFilled5OuterV5InnerV3fooyyF"
    // TRANSITIVE: "availability": [
    // TRANSITIVE-NEXT:   {
    // TRANSITIVE-NEXT:     "domain": "macOS",
    // TRANSITIVE-NEXT:     "obsoleted": {
    // TRANSITIVE-NEXT:       "major": 50,
    // TRANSITIVE-NEXT:       "minor": 1
    // TRANSITIVE-NEXT:     }
    // TRANSITIVE-NEXT:   }
    // TRANSITIVE-NEXT: ]
    public func foo() {}
  }
}
