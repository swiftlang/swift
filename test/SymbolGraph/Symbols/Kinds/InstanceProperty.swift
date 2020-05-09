// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InstanceProperty -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InstanceProperty -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InstanceProperty.symbols.json

public struct S {
  // CHECK: "identifier": "swift.property"
  // CHECK-NEXT: "displayName": "Instance Property"
  // CHECK: pathComponents
  // CHECK: "S"
  // CHECK-NEXT: "x"
  public var x: Int
}
