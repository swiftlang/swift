func foo() {
    let a = 3
    if a > 2 {
        if a < 10 {}
    }
}
// RUN: %refactor -source-filename %s -pos=3:5 | %FileCheck %s -check-prefix=CHECK-COLLAPSE-NESTED-IF-EXPRESSION
// CHECK-COLLAPSE-NESTED-IF-EXPRESSION: Collapse Nested If Expression
