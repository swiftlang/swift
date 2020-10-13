// RUN: not %target-swift-frontend -typecheck %s

func addProperties(b: Int) {
    guard true else {
      _ = "`\(b)`."
