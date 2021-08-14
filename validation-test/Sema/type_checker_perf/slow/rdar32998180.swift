// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

func rdar32998180(value: UInt16) -> UInt16 {
  let result = ((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1)
  | (((((value >> 1) ^ (value >> 1) ^ (value >> 1) ^ (value >> 1)) & 1) << 1) << 1)
  // expected-error@-1 {{the compiler is unable to type-check this expression in reasonable time}}
  return result
}
