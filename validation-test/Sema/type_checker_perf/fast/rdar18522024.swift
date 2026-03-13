// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

func test(a: [String], b: String, c: String) -> [String] {
  return a.map { $0 + ": " + b + "(" + c + $0 + ")" }
}
