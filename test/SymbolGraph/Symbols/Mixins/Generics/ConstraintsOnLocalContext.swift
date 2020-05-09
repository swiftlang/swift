// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ConstraintsOnLocalContext -emit-module-path %t/ConstraintsOnLocalContext.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name ConstraintsOnLocalContext -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ConstraintsOnLocalContext.symbols.json

public func foo<T: Sequence>(x: T) {}

// CHECK: swiftGenerics
// CHECK-NEXT: "parameters": [
//               {
// CHECK:          "name":  "T"
// CHECK-NEXT:     "index": 0
// CHECK-NEXT:     "depth": 0
// CHECK-NEXT:   }
// CHECK-NEXT: ]
// CHECK-NEXT: "constraints": [
//               {
// CHECK:          "kind": "conformance"
// CHECK-NEXT:     "lhs": "T"
// CHECK-NEXT:     "rhs": "Sequence"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
