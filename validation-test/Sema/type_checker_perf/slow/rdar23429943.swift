// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let _ = [0].reduce([Int]()) {
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
  return $0.count == 0 && $1 == 0 ? [] : $0 + [$1]
}
