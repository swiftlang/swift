// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=300
// REQUIRES: tools-release,no_asan

// Selecting operators from the closure before arguments to `zip` makes this "too complex"
func compute(_ ids: [UInt64], two: UInt64, zero: UInt64, one: UInt64, answer: UInt64) {
  let _ = zip(ids[ids.indices.dropLast()], ids[ids.indices.dropFirst()]).map { pair in
    ((pair.0 % two == zero) && (pair.1 % two == one)) ? UInt64(pair.1 - pair.0) : answer
  }
}
