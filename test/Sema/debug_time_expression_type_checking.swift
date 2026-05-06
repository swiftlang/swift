// Regression test: -debug-time-expression-type-checking must emit per-expression
// timing lines to stderr. Prior to the fix, startExpressionTimer() would return
// early when ExpressionTimeoutThreshold == 0, suppressing all output.
//
// RUN: %target-swiftc_driver -Xfrontend -debug-time-expression-type-checking %s 2>&1 | %FileCheck %s
// CHECK: ms	{{.*}}debug_time_expression_type_checking.swift:

func example() {
    let x = [1, 2, 3].map { $0 * 2 }
    let _ = x.reduce(0, +)
}
