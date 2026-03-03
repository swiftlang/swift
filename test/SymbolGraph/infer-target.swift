// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Output)
// RUN: %target-build-swift %s -module-name Basic -emit-module -emit-module-path %t/

// Verify that -target can be inferred
// RUN: %empty-directory(%t/Output)
// RUN: %swift-symbolgraph-extract -module-name Basic -I %t -pretty-print -output-dir %t/Output
// RUN: %FileCheck %s --input-file %t/Output/Basic.symbols.json

// This test can only work for test configurations that aren't cross-compiling.
// REQUIRES: target-same-as-host

public struct S {
  public var x: Int
}

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:5Basic1SV1xSivp"
// CHECK-NEXT: "target": "s:5Basic1SV"
