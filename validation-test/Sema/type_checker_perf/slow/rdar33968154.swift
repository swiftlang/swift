// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let a = 0
let b = 42
let c: Float = 0.0
let d: Float = 42.0
let n = 1

let _ = ((a * c) + ((b * d - a * c) / (b - a)) * (n - a)) / n
