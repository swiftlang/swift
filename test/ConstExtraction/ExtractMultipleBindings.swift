// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractMultipleBindings.swiftconstvalues -const-gather-top-level-constant first -const-gather-top-level-constant second -const-gather-top-level-constant third -const-gather-top-level-constant left -const-gather-top-level-constant right -primary-file %s
// RUN: cat %t/ExtractMultipleBindings.swiftconstvalues 2>&1 | %FileCheck %s

let first = 1, second = 2, third = 3

let (left, right) = (10, 20)

// CHECK:        "label": "first",
// CHECK:        "valueKind": "RawLiteral",
// CHECK-NEXT:   "value": "1"

// CHECK:        "label": "second",
// CHECK:        "valueKind": "RawLiteral",
// CHECK-NEXT:   "value": "2"

// CHECK:        "label": "third",
// CHECK:        "valueKind": "RawLiteral",
// CHECK-NEXT:   "value": "3"

// CHECK:        "label": "left",
// CHECK:        "valueKind": "Runtime"

// CHECK:        "label": "right",
// CHECK:        "valueKind": "Runtime"
