// RUN: %target-typecheck-verify-swift -solver-scope-threshold=5000
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

// FIXME: Array literals are considered "speculative" bindings at the moment but they cannot actually
// assume different types unlike integer and floating-pointer literals.
func f(n: Int, a: [Int]) {
  // expected-error@+1 {{the compiler is unable to type-check this expression in reasonable time}}
  let _ = [(0 ..< n + a.count).map { Int8($0) }] +
          [(0 ..< n + a.count).map { Int8($0) }.reversed()] // Ok
}
