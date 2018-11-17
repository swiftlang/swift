// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

let _ = Array([0].lazy.reversed().filter { $0 % 2 == 0 }.map { $0 / 2 })
