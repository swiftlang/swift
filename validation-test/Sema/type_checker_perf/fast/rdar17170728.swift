// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 5 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

let i: Int? = 1
let j: Int?
let k: Int? = 2

let _ = [i, j, k].reduce(0 as Int?) {
  $0 != nil && $1 != nil ? $0! + $1! : ($0 != nil ? $0! : ($1 != nil ? $1! : nil))
}
