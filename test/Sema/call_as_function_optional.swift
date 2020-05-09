// RUN: %target-typecheck-verify-swift

// Test extending `Optional` with `func callAsFunction`.
// `Optional` is an edge case since constraint simplification has special
// `Optional` stripping logic.

extension Optional {
  func callAsFunction() -> Optional {
    return self
  }
  func callAsFunction(_ fn: (Int) -> Void) -> Optional {
    return self
  }
}
func testOptional<T>(_ x: T?) {
  _ = x()()()
  // Test trailing closure syntax.
  _ = x { _ in } ({ _ in })
}
