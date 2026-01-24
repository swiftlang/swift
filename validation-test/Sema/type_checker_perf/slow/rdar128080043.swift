// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000

// Invalid expression, because there is no (Int, Double) overload of -.
//
// This used to produce the correct diagnostic in 6.2, but became too
// complex in 6.3.

func slow(B: Int, F: Double) {
  let _ = B - Double(B) - Double(0.0 * Double(B)) + F
  // expected-error@-1 {{reasonable time}}
}
