// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let a = "a"
let b = "b"
let c = 42
_ = "a=" + a + ";b=" + b + ";c=" + c
// expected-error@-1 {{reasonable time}}
