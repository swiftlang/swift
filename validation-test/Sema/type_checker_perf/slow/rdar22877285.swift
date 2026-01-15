// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing overloads of +

let j = 1

_ = "a" + j + "b" + j + "c" + j + "d" + j
// expected-error@-1 {{reasonable time}}
