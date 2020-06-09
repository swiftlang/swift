// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConstraintsOnOuterContext -emit-module-path %t/ConstraintsOnOuterContext.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name ConstraintsOnOuterContext -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ConstraintsOnOuterContext.symbols.json

public struct MyStruct<S: Sequence> {
  public var x: S
  public init(x: S) {
    self.x = x
  }
  public func foo() where S.Element == Int {}
}

// CHECK-LABEL: "precise": "s:25ConstraintsOnOuterContext8MyStructV3fooyySi7ElementRtzrlF"
// CHECK: swiftGenerics
// CHECK: "constraints": [
//               {
// CHECK:          "kind": "conformance"
// CHECK-NEXT:     "lhs": "S"
// CHECK-NEXT:     "rhs": "Sequence"
//               },
//               {
// CHECK:          "kind": "sameType"
// CHECK-NEXT:     "lhs": "S.Element"
// CHECK-NEXT:     "rhs": "Int"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
