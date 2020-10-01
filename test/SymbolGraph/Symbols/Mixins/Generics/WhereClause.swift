// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name WhereClause -emit-module-path %t/WhereClause.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name WhereClause -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/WhereClause.symbols.json

public func foo<T>(x: T) where T: Sequence {}

// CHECK: swiftGenerics
// CHECK-NEXT: "parameters": [
//               {
// CHECK: "name":  "T"
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
