// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Operator -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Operator -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Operator.symbols.json

public struct S {
  // CHECK: "identifier": "swift.func.op"
  // CHECK-NEXT: "displayName": "Operator"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "+(_:_:)"
  public static func +(lhs: S, rhs: S) -> S {
    return lhs
  }
}
