// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func rdar32998180(value: UInt16) -> UInt16 {
  let result = ((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1)
  | (((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1) << 1)
  return result
}
