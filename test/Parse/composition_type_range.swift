// RUN: not %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s

typealias A = B & protocol<C, D>
// CHECK: (typealias range=[{{.+}}.swift:[[@LINE-1]]:1 - line:[[@LINE-1]]:32] "A" {{.*}})
