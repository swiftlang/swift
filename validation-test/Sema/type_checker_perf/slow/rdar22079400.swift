// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

let _ = (0...1).lazy.flatMap {
  a in (1...2).lazy.map { b in (a, b) }
}.filter {
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
  1 < $0 && $0 < $1 && $0 + $1 < 3
}
