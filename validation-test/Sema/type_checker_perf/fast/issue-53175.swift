// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

// https://github.com/swiftlang/swift/issues/53175

func slow() {
  if 0.1 * 5 * 5 * 5 * 5 * 5 * 5 == 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 + 0.1 { }
}
