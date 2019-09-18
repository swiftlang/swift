// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let _ = Array([0].lazy.reversed().filter { $0 % 2 == 0 }.map { $0 / 2 })
