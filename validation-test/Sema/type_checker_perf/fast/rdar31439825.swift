// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let a = 1

_ = -a + -a - -a + -a - -a
