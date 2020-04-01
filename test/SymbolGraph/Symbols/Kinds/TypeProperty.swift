// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TypeProperty -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TypeProperty -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TypeProperty.symbols.json

public struct S {
  // CHECK: "identifier": "swift.type.property"
  // CHECK-NEXT: "displayName": "Type Property"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "y"
  public static let y = 2
}
