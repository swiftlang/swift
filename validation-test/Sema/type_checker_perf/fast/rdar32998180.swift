// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

func rdar32998180(value: UInt16) -> UInt16 {
  let result = ((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1)
             | (((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1) << 1)
  return result
}
