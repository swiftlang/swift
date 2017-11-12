// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

func test(n: Int) -> Int {
  return n == 0 ? 0 : (0..<n).reduce(0) {
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
    ($0 > 0 && $1 % 2 == 0) ? (($0 + $1) / ($1 - $0)) : $0
  }
}
