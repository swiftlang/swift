// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IntroducedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name IntroducedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IntroducedFilled.symbols.json
// RUN: %FileCheck %s --input-file %t/IntroducedFilled.symbols.json --check-prefix=TRANSITIVE

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.15)
public struct S {
  // CHECK-LABEL: "precise": "s:16IntroducedFilled1SV3fooyyF",
  // CHECK: "availability": [
  // CHECK-NEXT:   {
  // CHECK-NEXT:     "domain": "macOS",
  // CHECK-NEXT:     "introduced": {
  // CHECK-NEXT:       "major": 10,
  // CHECK-NEXT:       "minor": 15
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
  // CHECK-NEXT: ]
  public func foo() {}
}

@available(macOS, introduced: 10.15)
public struct Outer {
  public struct Inner {
    // TRANSITIVE-LABEL: "precise": "s:16IntroducedFilled5OuterV5InnerV3fooyyF"
    // TRANSITIVE: "availability": [
    // TRANSITIVE-NEXT:   {
    // TRANSITIVE-NEXT:     "domain": "macOS",
    // TRANSITIVE-NEXT:     "introduced": {
    // TRANSITIVE-NEXT:       "major": 10,
    // TRANSITIVE-NEXT:       "minor": 15
    // TRANSITIVE-NEXT:     }
    // TRANSITIVE-NEXT:   }
    // TRANSITIVE-NEXT: ]
    public func foo() {}
  }
}
