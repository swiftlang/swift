// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
func f(tail: UInt64, byteCount: UInt64) {
  if tail & ~(1 << ((byteCount & 7) << 3) - 1) == 0 { }
}
