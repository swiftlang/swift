// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

// Mixed UInt32 and Double
let x: UInt32 = 1
let _ = x > (33 + 55 + 6 + 55 + 6 + 55 + 6 + 55 + 6 + 27.5)
// expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
