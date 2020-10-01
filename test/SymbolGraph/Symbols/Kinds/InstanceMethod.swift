// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InstanceMethod -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InstanceMethod -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InstanceMethod.symbols.json

public struct S {
  // CHECK: "identifier": "swift.method"
  // CHECK-NEXT: "displayName": "Instance Method"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "foo()"
  public func foo() {}
}
