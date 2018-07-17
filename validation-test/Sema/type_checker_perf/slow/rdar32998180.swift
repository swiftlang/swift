// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 4
// REQUIRES: tools-release,no_asserts

func rdar32998180(value: UInt16) -> UInt16 {
  var result = ((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1)
  | (((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1) << 1)
  // expected-error@-1 {{reasonable time}}
  return result
}
