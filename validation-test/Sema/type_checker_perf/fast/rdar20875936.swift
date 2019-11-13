// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let _ = Array([0].lazy.reversed().filter { $0 % 2 == 0 }.map { $0 / 2 })
