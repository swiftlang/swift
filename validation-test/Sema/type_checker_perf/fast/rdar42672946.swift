// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -swift-version 5 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
func f(tail: UInt64, byteCount: UInt64) {
  if tail & ~(1 << ((byteCount & 7) << 3) - 1) == 0 { }
}
