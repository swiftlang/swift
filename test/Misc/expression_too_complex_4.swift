// RUN: %target-typecheck-verify-swift -swift-version 4 -solver-expression-time-threshold=1 -warn-long-expression-type-checking=1 -solver-memory-threshold 1500000000

func test(_ i: Int, _ j: Int) -> Int {
  return 1 + (((i >> 1) + (i >> 2) + (i >> 3) + (i >> 4) << 1) << 1) & 0x40
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
}
