// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression, because there is no (A, Int) overload of /

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
