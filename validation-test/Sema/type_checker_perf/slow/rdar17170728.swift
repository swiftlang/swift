// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let i: Int? = 1
let j: Int?
let k: Int? = 2

let _ = [i, j, k].reduce(0 as Int?) {
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
  $0 != nil && $1 != nil ? $0! + $1! : ($0 != nil ? $0! : ($1 != nil ? $1! : nil))
}
