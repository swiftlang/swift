// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

func test() {
  let _ = (1.0 / ((20 + (8.0 / 10.0) + (1.0 / 30.0)))) * (1.0 / 240.0) * 60.0
}
