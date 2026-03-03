// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

func test(_ d: Double) -> Double {
  return d + d - d - (d / 2) + (d / 2) + (d / 2.0)
}
