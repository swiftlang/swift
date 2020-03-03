// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name AssociatedType -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name AssociatedType -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/AssociatedType.symbols.json

public protocol P {
  // CHECK: "identifier": "swift.associatedtype"
  // CHECK-NEXT: "displayName": "Associated Type"
  // CHECK: pathComponents
  // CHECK-NEXT: "P"
  // CHECK-NEXT: "Thing"
  associatedtype Thing
}


