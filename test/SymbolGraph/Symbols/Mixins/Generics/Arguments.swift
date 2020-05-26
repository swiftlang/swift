// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Arguments -emit-module-path %t/Arguments.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Arguments -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Arguments.symbols.json

public struct MyStruct<T> {
  public var x: T
  public init(x: T) {
    self.x = x
  }
}

// CHECK: swiftGenerics
// CHECK-NEXT: "parameters": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "name":  "T"
// CHECK-NEXT:     "index": 0
// CHECK-NEXT:     "depth": 0
// CHECK-NEXT:   }
// CHECK-NEXT: ]
// CHECK-NOT: constraints
