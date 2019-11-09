// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release

func test(a: [String], b: String, c: String) -> [String] {
  return a.map { $0 + ": " + b + "(" + c + $0 + ")" }
}
