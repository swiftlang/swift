// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// This type checks quickly in 6.2, regresses in 6.3.

func slow() -> [Double] {
  // expected-error@+1 {{reasonable time}}
  return [1/8.0, 1/4.0, 1/3.0, 1/2.0, 2/3.0, 3/4.0, 1, 5/4.0, 4/3.0, 3/2.0, 2, 4, 8]
    .map{$0/8.0}
}
