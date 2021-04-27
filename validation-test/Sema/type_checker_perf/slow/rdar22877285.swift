// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let j = 1

_ = "a" + j + "b" + j + "c" + j + "d" + j
// expected-error@-1 {{reasonable time}}
