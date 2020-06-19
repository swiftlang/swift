// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnavailableFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnavailableFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnavailableFilled.symbols.json

// REQUIRES: OS=macosx

@available(iOS, unavailable)
public struct S {
  public func foo() {}
}

// CHECK-LABEL: "precise": "s:17UnavailableFilled1SV3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "iOS",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]
