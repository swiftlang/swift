// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Init -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Init -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Init.symbols.json

public struct S {
  public var x: Int

  // CHECK: "identifier": "swift.init"
  // CHECK-NEXT: "displayName": "Initializer"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "init(x:)"
  public init(x: Int) {
    self.x = x
  }
}
