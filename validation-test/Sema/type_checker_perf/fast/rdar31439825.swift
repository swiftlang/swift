// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

let a = 1

_ = -a + -a - -a + -a - -a
