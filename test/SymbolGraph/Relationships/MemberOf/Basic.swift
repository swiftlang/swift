// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Output)
// RUN: %target-build-swift %s -module-name Basic -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Basic -I %t -pretty-print -output-dir %t/Output
// RUN: %FileCheck %s --input-file %t/Output/Basic.symbols.json

public struct S {
  public var x: Int
}

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:5Basic1SV1xSivp"
// CHECK-NEXT: "target": "s:5Basic1SV"
