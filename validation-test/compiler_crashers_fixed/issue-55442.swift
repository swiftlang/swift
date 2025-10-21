// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/55442

func addProperties(b: Int) {
    guard true else {
      _ = "`\(b)`."
