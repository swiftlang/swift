// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let a = 1

_ = -a + -a - -a + -a - -a
// expected-error@-1 {{reasonable time}}
