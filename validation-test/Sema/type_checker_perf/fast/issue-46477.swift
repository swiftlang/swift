// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000

func foo(_ string: String) -> Int {
  let bar = Array(string)
    .reversed()
    .enumerated()
    .map { ((1 << ($0 + 1)) % 11) * Int(String($1))! }
    .reduce(0) { $0 + $1 }
  return (12 - bar  % 11) % 11
}
