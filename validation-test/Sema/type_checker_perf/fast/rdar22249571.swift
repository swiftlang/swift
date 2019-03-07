// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

func rdar22249571() -> [UInt8] {
  return (0...10).map { _ in
    UInt8(Int(1) % Int(1))
  }
}
