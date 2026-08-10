// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/out.swiftconstvalues -const-gather-top-level-constant greeting %t/main.swift
// RUN: cat %t/out.swiftconstvalues 2>&1 | %FileCheck %s

let greeting = "hello"

// CHECK:      [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "topLevelConstant",
// CHECK-NEXT:     "label": "greeting",
// CHECK-NEXT:     "type": "Swift.String",
// CHECK-NEXT:     "mangledTypeName": "n/a - deprecated",
// CHECK-NEXT:     "isStatic": "false",
// CHECK-NEXT:     "isComputed": "false",
// CHECK-NEXT:     "file": "{{.*}}main.swift",
// CHECK-NEXT:     "line": 6,
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": "hello"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
