// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name MemberOf -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name MemberOf -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/MemberOf.symbols.json

public struct S {
  public var x: Int
}

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:8MemberOf1SV1xSivp"
// CHECK-NEXT: "target": "s:8MemberOf1SV"
