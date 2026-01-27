// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

var counts = [ 0, 0, 0, 0 ]

// Invalid expression: Missing (Int, Array<Int>) overload of +

let d = counts[0] * 1000 + counts[1] * 100 + counts[2] * 10 + counts
// expected-error@-1 {{the compiler is unable to type-check this expression in reasonable time; try breaking up the expression into distinct sub-expressions}}
