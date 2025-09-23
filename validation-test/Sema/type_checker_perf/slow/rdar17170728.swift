// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

let i: Int? = 1
let j: Int?
let k: Int? = 2

// expected-error@+1 {{the compiler is unable to type-check this expression in reasonable time}}
let _ = [i, j, k].reduce(0 as Int?) {
  $0 != nil && $1 != nil ? $0! + $1! : ($0 != nil ? $0! : ($1 != nil ? $1! : nil))
}
