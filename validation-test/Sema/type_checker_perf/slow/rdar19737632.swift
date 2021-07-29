// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let a = "a"
let b = "b"
let c = 42
let d = 0.0
let e: Float = 1.0

_ = "a=" + a + ";b=" + b + ";c=" + c + ";d=" + d + ";e=" + e
// expected-error@-1 {{reasonable time}}
