// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

struct S {
  let s: String
}

func rdar22282851(_ a: [S]) -> [S] {
  let result = a.filter { $0.s == "hello" }.sorted { $0.s < $1.s || ($0.s == $1.s && $0.s < $1.s) && $0.s >= $1.s }
  return result
}
