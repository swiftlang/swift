// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 5 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func test(n: Int) -> Int {
  return n == 0 ? 0 : (0..<n).reduce(0) {
    ($0 > 0 && $1 % 2 == 0) ? ((($0 + $1) - ($0 + $1)) / ($1 - $0)) + (($0 + $1) / ($1 - $0)) : $0
  }
}
