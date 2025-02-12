// RUN: %target-typecheck-verify-swift -solver-scope-threshold=5000

// REQUIRES: tools-release,no_asan

func test(_ ids: [UInt64]) {
  _ = zip(ids[ids.indices.dropLast()], ids[ids.indices.dropFirst()]).map { pair in
    ((pair.0 % 2 == 0) && (pair.1 % 2 == 1)) ? UInt64(pair.1 - pair.0) : 42
  }
}
