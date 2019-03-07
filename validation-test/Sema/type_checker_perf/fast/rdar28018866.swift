// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

struct P {
  let x: Float
  let y: Float
}

func rdar28018866(pt: P, p0: P, p1: P) -> Bool {
  return (pt.x - p0.x) * (p1.y - p0.y) - (pt.y - p0.y) * (p1.x - p0.x) < 0.0
}
