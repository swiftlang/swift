// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

func test() {
  let _ = (1 + 2) + (3 + 4) + (5 + 6) + 0.5
  let _ = (1 + 2) + (3 + 4 + 5 + 6) + 0.5
  let _ = 1 + 2 + 3 + 4 + 5 + 6 + 0.5
  let _ = (1 + 2) + (3 + 4) + 0.5
  let _ = (1.0 + 2.0) + (3.0 + 4.0) + (5.0 + 6.0) + 0.5
}
