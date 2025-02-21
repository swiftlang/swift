// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

// The test takes about 63k scopes here due to the fact
// that prefix `-` operator doesn't have an overload that
// take `Int`. We could fix that in the stdlib.

func rdar31742586() -> Double {
  return -(1 + 2) + -(3 + 4) + 5 - (-(1 + 2) + -(3 + 4) + 5)
  // expected-error@-1 {{the compiler is unable to type-check this expression in reasonable time}}
}
