// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func test(a: [String], b: String, c: String) -> [String] {
  return a.map { $0 + ": " + b + "(" + c + $0 + ")" }
}
