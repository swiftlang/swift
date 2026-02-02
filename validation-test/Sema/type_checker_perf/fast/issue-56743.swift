// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// https://github.com/swiftlang/swift/issues/56743

func slow() {
  _ = Double(1 * 1 + 2 * 0 + -1 * 12) / Double(1 + 2 + 12)
}
