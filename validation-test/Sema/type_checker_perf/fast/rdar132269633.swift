// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200

func test() {
  let _: Float = (1 * 3 + 2 * 2 + 3 * 1) * 9 + 42
}
