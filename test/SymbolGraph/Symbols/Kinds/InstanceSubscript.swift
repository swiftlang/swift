// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InstanceSubscript -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InstanceSubscript -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InstanceSubscript.symbols.json

public struct S {
  // CHECK: "identifier": "swift.subscript"
  // CHECK-NEXT: "displayName": "Instance Subscript"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "subscript(_:)"
  public subscript(i: Int) -> Int {
    return 2
  }
}
