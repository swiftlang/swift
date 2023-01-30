// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=5
// REQUIRES: tools-release,no_asan

// Hits the default memory threshold
// https://github.com/apple/swift/issues/43369

struct A {}
struct B {}

func - (lhs: A, rhs: A) -> B {}
func / (lhs: B, rhs: Double) -> B {}
func - (lhs: B, rhs: B) -> B {}

do {
  let a0 = A()
  let a1 = A()

  let t0 = 0
  let t1 = 0

  // expected-error@+1 {{the compiler is unable to type-check this expression in reasonable time}}
  let _ = (a0 - a0) / (t0 - t0) - (a1 - a1) / (t1 - t1)
}
