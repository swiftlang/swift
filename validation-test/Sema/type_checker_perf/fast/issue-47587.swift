// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://github.com/swiftlang/swift/issues/47587

func slow() {
  let _ = ["x"].count > 6 + 1 + 4 + 1 + 40 + 1
}
