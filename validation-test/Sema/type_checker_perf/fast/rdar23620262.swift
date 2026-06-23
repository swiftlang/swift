// RUN: %target-typecheck-verify-swift -solver-scope-threshold=15000
// REQUIRES: tools-release,no_asan

func test() {
  let a: [Double] = []
  _ = a.map { $0 - 1.0 }
    .map { $0 * $0 }
    .reduce(0, +) / Double(a.count)
}
