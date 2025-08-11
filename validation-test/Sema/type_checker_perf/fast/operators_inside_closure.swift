// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=10000
// REQUIRES: tools-release,no_asan

// Selecting operators from the closure before arguments to `zip` makes this "too complex"
func compute(_ ids: [UInt64]) {
  let _ = zip(ids[ids.indices.dropLast()], ids[ids.indices.dropFirst()]).map { pair in
    ((pair.0 % 2 == 0) && (pair.1 % 2 == 1)) ? UInt64(pair.1 - pair.0) : 42
  }
}
