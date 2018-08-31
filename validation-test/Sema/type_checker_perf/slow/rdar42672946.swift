// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
func f(tail: UInt64, byteCount: UInt64) {
  if tail & ~(1 << ((byteCount & 7) << 3) - 1) == 0 { }
  // expected-error@-1 {{reasonable time}}
}
