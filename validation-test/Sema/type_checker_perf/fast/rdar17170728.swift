// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000 -solver-enable-extreme-edge-contraction -solver-enable-promote-supertypes
// REQUIRES: tools-release,no_asan

let i: Int? = 1
let j: Int? = 1
let k: Int? = 2

let _ = [i, j, k].reduce(0 as Int?) {
  $0 != nil && $1 != nil ? $0! + $1! : ($0 != nil ? $0! : ($1 != nil ? $1! : nil))
}
