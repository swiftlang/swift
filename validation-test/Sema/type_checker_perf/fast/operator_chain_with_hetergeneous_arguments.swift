// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

// TODO(performance): This should be converted in a scale test once performance issue is fixed
func test(bytes: Int, length: UInt32) {
  // left-hand side of `>=` is `Int` and right-hand side is a chain of `UInt32` inferred from `length`
  _ = bytes >= 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + length
}
