// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing (UInt32, Double) overload of >

let x: UInt32 = 1
let _ = x > (33 + 55 + 6 + 55 + 6 + 55 + 6 + 55 + 6 + 27.5)
// expected-error@-1 {{reasonable time}}
