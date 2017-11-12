// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

struct S {
  let s: String
}

func rdar22282851(_ a: [S]) -> [S] {
  let result = a.sorted{ $0.s < $1.s || ($0.s == $1.s && $0.s < $1.s) }
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
  return result
}
